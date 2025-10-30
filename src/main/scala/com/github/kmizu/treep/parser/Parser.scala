package com.github.kmizu.treep.parser

import com.github.kmizu.treep.lexer.*
import com.github.kmizu.treep.parser.CST.*
import com.github.kmizu.treep.parser.CST as C
import java.util.concurrent.atomic.AtomicReference

object Parser:
  final case class ParseDiag(message: String, line: Int, col: Int)

  private val recentErrors = new AtomicReference[List[ParseDiag]](Nil)
  def lastErrors: List[ParseDiag] = recentErrors.get()

  def parseProgram(src: String, file: String = "<stdin>"): Program =
    val tokens = Lexer.tokenize(src, file).toVector
    val p = new P(tokens, file)
    val prog = p.program()
    recentErrors.set(p.errors.toList)
    prog

  def parseProgramWithDiagnostics(src: String, file: String = "<stdin>"): (Program, List[ParseDiag]) =
    val tokens = Lexer.tokenize(src, file).toVector
    val p = new P(tokens, file)
    val prog = p.program()
    val errs = p.errors.toList
    recentErrors.set(errs)
    (prog, errs)

  private final class P(tokens: Vector[Token], fileName: String):
    private var i = 0
    val errors = scala.collection.mutable.ListBuffer.empty[ParseDiag]

    final case class ParseException(msg: String, line: Int, col: Int) extends RuntimeException(msg)

    private def cur: Token = if i < tokens.length then tokens(i) else tokens.last
    private def at(k: String): Boolean = cur.kind == k
    private def eat(k: String): Token =
      if at(k) then { val t = cur; i += 1; t } else error(s"expected ${k}, found ${cur.kind}")
    private def next(): Token = { val t = cur; i += 1; t }
    private def report(msg: String): Unit = errors += ParseDiag(msg, cur.line, cur.col)
    private def error(msg: String): Nothing =
      val e = ParseException(msg, cur.line, cur.col)
      report(msg)
      throw e

    private def synchronizeTop(): Unit =
      val sync = Set("DEF", "CONST", "MODULE", "EXTENSION", "MACRO", "EOF")
      while !sync.contains(cur.kind) do i += 1

    private def synchronizeStmt(): Unit =
      val starters = Set("LET", "RETURN", "IF", "WHILE", "FOR", "}", "EOF")
      while !starters.contains(cur.kind) do i += 1

    def program(): Program =
      val tops = scala.collection.mutable.ListBuffer.empty[TopDecl]
      while !at("EOF") do
        try
          cur.kind match
            case "DEF"       => tops += funDef()
            case "CONST"     => tops += constDecl()
            case "MODULE"    => tops += moduleDecl()
            case "STRUCT"    => tops += structDef()
            case "EXTENSION" => tops += extensionDecl()
            case "MACRO"     => tops += macroDef()
            case other     =>
              report(s"unexpected token at top-level: ${other}")
              synchronizeTop()
        catch
          case _: ParseException => synchronizeTop()
      Program(tops.toList)

    private def moduleDecl(): TopDecl =
      val tok = eat("MODULE")
      val name = ident()
      val body = block()
      ModuleDecl(name, body, span = Some(C.Span(fileName, tok.line, tok.col)))

    private def constDecl(): TopDecl =
      val tok = eat("CONST")
      val name = ident()
      val annot = if at(":") then { next(); Some(typeAnnot()) } else None
      eat("=")
      val init = expr()
      ConstDecl(name, annot, init, span = Some(C.Span(fileName, tok.line, tok.col)))

    private def funDef(): TopDecl =
      val tok = eat("DEF")
      val name = ident()
      eat("(")
      val params =
        if at(")") then { next(); Nil }
        else
          val ps = scala.collection.mutable.ListBuffer.empty[Param]
          ps += param()
          while at(",") do { next(); ps += param() }
          eat(")"); ps.toList
      val ret =
        if at("RETURNS") then { next(); eat(":"); Some(typeAnnot()) } else None
      val body = block()
      FunDef(name, params, ret, body, span = Some(C.Span(fileName, tok.line, tok.col)))

    private def structDef(): TopDecl =
      val tok = eat("STRUCT")
      val name = ident()
      eat("{")
      val fields = scala.collection.mutable.ListBuffer.empty[(String, TypeAnnot)]
      if !at("}") then
        fields += fieldDecl()
        while at(",") do { next(); fields += fieldDecl() }
      eat("}")
      C.StructDef(name, fields.toList, span = Some(C.Span(fileName, tok.line, tok.col)))

    private def extensionDecl(): TopDecl =
      val tok = eat("EXTENSION")
      eat("(")
      val receiverParam = ident()
      eat(":")
      val receiverType = typeAnnot()
      eat(")")
      eat("{")
      val methods = scala.collection.mutable.ListBuffer.empty[FunDef]
      while !at("}") do
        try
          if at("DEF") then
            funDef() match
              case f: FunDef => methods += f
              case other => error(s"expected FunDef, got ${other}")
          else error(s"expected def in extension block, found ${cur.kind}")
        catch
          case _: ParseException =>
            synchronizeStmt()
      eat("}")
      ExtensionDecl(receiverParam, receiverType, methods.toList, span = Some(C.Span(fileName, tok.line, tok.col)))

    private def macroDef(): TopDecl =
      val tok = eat("MACRO")
      val name = ident()
      eat("{")

      // Parse pattern: <pattern text>
      eat("PATTERN")
      eat(":")
      val patternStart = i
      var patternText = ""
      // Collect tokens until we see "expand" or "}"
      while !at("EXPAND") && !at("}") && !at("EOF") do
        patternText += cur.lexeme
        if !at("EXPAND") && !at("}") then patternText += " "
        next()
      patternText = patternText.trim

      // Parse expand: <block>
      var expansionBlock = Block(Nil)
      if at("EXPAND") then
        eat("EXPAND")
        eat(":")
        expansionBlock = block()

      eat("}")
      MacroDef(name, patternText, expansionBlock, span = Some(C.Span(fileName, tok.line, tok.col)))

    private def fieldDecl(): (String, TypeAnnot) =
      val n = ident(); eat(":"); (n, typeAnnot())

    private def param(): Param =
      val name = ident(); eat(":"); Param(name, typeAnnot())

    private def typeAnnot(): TypeAnnot =
      parseArrowType()

    // Right-associative function type: A -> B -> C  == A -> (B -> C)
    private def parseArrowType(): TypeAnnot =
      var left = parseTypeAtom()
      while at("->") do
        next()
        val right = parseArrowType()
        left = TypeAnnot("->", List(left, right))
      left

    private def parseTypeAtom(): TypeAnnot =
      val base = ident()
      if at("[") then
        next()
        val args = scala.collection.mutable.ListBuffer.empty[TypeAnnot]
        args += typeAnnot()
        while at(",") do { next(); args += typeAnnot() }
        eat("]")
        TypeAnnot(base, args.toList)
      else TypeAnnot(base)

    private def block(): Block =
      val tok = eat("{")
      val ss = scala.collection.mutable.ListBuffer.empty[Stmt]
      while !at("}") do
        try ss += stmt()
        catch
          case _: ParseException => synchronizeStmt()
      eat("}")
      Block(ss.toList, span = Some(C.Span(fileName, tok.line, tok.col)))

    private def stmt(): Stmt =
      if at("LET") then
        val tok = next();
        val name = ident()
        val annot = if at(":") then { next(); Some(typeAnnot()) } else None
        eat("=")
        val init = expr()
        LetStmt(name, annot, init, span = Some(C.Span(fileName, tok.line, tok.col)))
      else if at("RETURN") then
        val tok = next(); ReturnStmt(expr(), span = Some(C.Span(fileName, tok.line, tok.col)))
      else if at("IF") then ifStmt()
      else if at("WHILE") then whileStmt()
      else if at("FOR") then forStmt()
      else if at("MATCH") then matchStmt()
      else
        val sl = cur.line; val sc = cur.col
        ExprStmt(expr(), span = Some(C.Span(fileName, sl, sc)))

    private def ifStmt(): Stmt =
      val tok = eat("IF"); eat("("); val c = expr(); eat(")");
      val t = block()
      val e = if at("ELSE") then { next(); Some(block()) } else None
      IfStmt(c, t, e, span = Some(C.Span(fileName, tok.line, tok.col)))

    private def whileStmt(): Stmt =
      val tok = eat("WHILE"); eat("("); val c = expr(); eat(")");
      val b = block(); WhileStmt(c, b, span = Some(C.Span(fileName, tok.line, tok.col)))

    private def forStmt(): Stmt =
      val tok = eat("FOR"); eat("(")
      val name = ident(); eat("IN"); if at(":") then next(); val it = expr(); eat(")")
      val b = block(); ForInStmt(name, it, b, span = Some(C.Span(fileName, tok.line, tok.col)))

    private def matchStmt(): Stmt =
      val tok = eat("MATCH"); eat("("); val tgt = expr(); eat(")");
      eat("{")
      val cs = scala.collection.mutable.ListBuffer.empty[C.Case]
      if !at("}") then
        cs += caseArm()
        while !at("}") do cs += caseArm()
      eat("}")
      C.MatchStmt(tgt, cs.toList, span = Some(C.Span(fileName, tok.line, tok.col)))

    private def caseArm(): C.Case =
      eat("CASE")
      val p = pattern()
      eat("=>")
      val body = block()
      C.Case(p, body)

    private def pattern(): C.Pattern = cur.kind match
      case "INT" => val v = cur.lexeme.toInt; next(); C.PInt(v)
      case "STRING" => val s = cur.lexeme; next(); C.PStr(s)
      case "TRUE" => next(); C.PBool(true)
      case "FALSE" => next(); C.PBool(false)
      case "IDENT" =>
        val n = cur.lexeme; next(); if n == "_" then C.PWildcard else C.PVar(n)
      case _ =>
        // Treat unknown as wildcard for recovery
        next(); C.PWildcard

    // Pratt parser for expressions
    private enum BP(val v: Int):
      case ASSIGN extends BP(1)
      case OR extends BP(2)
      case AND extends BP(3)
      case CMP extends BP(4)
      case ADD extends BP(5)
      case MUL extends BP(6)
      case PREFIX extends BP(7)
      case POSTFIX extends BP(8)

    private def expr(): Expr = parseBin(BP.ASSIGN.v)

    private def parseBin(minBp: Int): Expr =
      var lhs = parseUnary()
      var loop = true
      while loop do
        val opTok = cur.kind
        val opInfo: Option[(Int, Int)] = opTok match
          case "||" => Some((BP.OR.v, BP.OR.v + 1))
          case "&&" => Some((BP.AND.v, BP.AND.v + 1))
          case "==" | "!=" | "<" | "<=" | ">" | ">=" => Some((BP.CMP.v, BP.CMP.v + 1))
          case "+" | "-" => Some((BP.ADD.v, BP.ADD.v + 1))
          case "*" | "/" | "%" => Some((BP.MUL.v, BP.MUL.v + 1))
          case "=" => Some((BP.ASSIGN.v, BP.ASSIGN.v - 1)) // right-assoc
          case _ => None
        opInfo match
          case Some((lbp, rbp)) if lbp >= minBp =>
            val op = cur.lexeme; next()
            val rhs = parseBin(rbp)
            lhs = Binary(op, lhs, rhs)
          case _ =>
            loop = false
      lhs

    private def parseUnary(): Expr =
      cur.kind match
        case "!" | "-" =>
          val op = cur.lexeme; next(); Unary(op, parseUnary())
        case _ => parsePostfix()

    private def parsePostfix(): Expr =
      var expr: Expr = parsePrimary()
      var cont = true
      while cont do
        if at(".") then {
          next()
          val n = ident()
          if at("(") then
            // method-call node: MethodCall(recv, name, args)
            next()
            val as = scala.collection.mutable.ListBuffer.empty[Expr]
            if !at(")") then
              as += this.expr()
              while at(",") do { next(); as += this.expr() }
            eat(")")
            expr = MethodCall(expr, n, as.toList)
          else
            expr = Field(expr, n)
        }
        else if at("[") then { next(); val k = this.expr(); eat("]"); expr = Index(expr, k) }
        else cont = false
      expr

    private def parsePrimary(): Expr = cur.kind match
      case "(" =>
        // lambda or grouped expr
        val saveI = i
        next()
        if at("IDENT") then
          // attempt param list: (x: T[, y: U]*) -> { ... }
          val paramsBuf = scala.collection.mutable.ListBuffer.empty[Param]
          val saveI2 = i
          val name = cur.lexeme; next()
          if at(":") then
            next();
            val tpe = typeAnnot()
            paramsBuf += Param(name, tpe)
            while at(",") do { next(); val n2 = ident(); eat(":"); paramsBuf += Param(n2, typeAnnot()) }
            if at(")") then
              next()
              if at("->") then
                next()
                val body = block(); return Lambda(paramsBuf.toList, body)
          // rollback if not lambda
          i = saveI2
        // group fallback
        val e = expr(); eat(")"); Group(e)
      case "INT" => val v = cur.lexeme.toInt; next(); IntLit(v)
      case "STRING" => val s = cur.lexeme; next(); StrLit(s)
      case "TRUE" => next(); BoolLit(true)
      case "FALSE" => next(); BoolLit(false)
      case "IDENT" =>
        val name = cur.lexeme; next()
        if at("(") then
          next()
          val as = scala.collection.mutable.ListBuffer.empty[Expr]
          if !at(")") then
            as += expr()
            while at(",") do { next(); as += expr() }
          eat(")")
          Call(name, as.toList)
        else Var(name)
      case "[" =>
        next()
        val elems = scala.collection.mutable.ListBuffer.empty[Expr]
        if !at("]") then
          elems += expr()
          while at(",") do { next(); elems += expr() }
        eat("]")
        ListLit(elems.toList)
      case "{" =>
        next()
        val pairs = scala.collection.mutable.ListBuffer.empty[(Expr, Expr)]
        if !at("}") then
          pairs += dictPair()
          while at(",") do { next(); pairs += dictPair() }
        eat("}")
        DictLit(pairs.toList)
      case k => error(s"unexpected primary: ${k}")

    private def dictPair(): (Expr, Expr) =
      val keyExpr = expr()
      eat(":")
      val v = expr()
      (keyExpr, v)

    private def ident(): String =
      if at("IDENT") then { val s = cur.lexeme; next(); s }
      else error("identifier expected")
