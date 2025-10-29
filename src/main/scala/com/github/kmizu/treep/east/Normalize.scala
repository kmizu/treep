package com.github.kmizu.treep.east

import com.github.kmizu.treep.parser.CST as C

object Normalize:
  // Normalize CST → EAST (stringly-typed attrs; structure via children)
  def toEAST(p: C.Program): Element =
    Element(
      kind = "module",
      name = Some("main"),
      children = p.tops.map(normTop)
    )

  private def normTop(td: C.TopDecl): Element = td match
    case C.FunDef(name, params, ret, body, sp) =>
      Element(
        kind = "def",
        name = Some(name),
        attrs = (if params.nonEmpty then List(Attr("params", params.map(pp => s"${pp.name}:${renderType(pp.tpe)}").mkString(","))) else Nil) ++
                ret.toList.map(r => Attr("returns", renderType(r))),
        children = List(normBlock(body)),
        span = sp.map(s => SourceSpan(s.file, s.line, s.col))
      )
    case C.ConstDecl(name, annot, init, sp) =>
      Element(
        kind = "const",
        name = Some(name),
        attrs = annot.toList.map(a => Attr("type", renderType(a))),
        children = List(Element("init", children = List(normExpr(init)))),
        span = sp.map(s => SourceSpan(s.file, s.line, s.col))
      )
    case C.ModuleDecl(name, body, sp) =>
      Element(kind = "module", name = Some(name), children = List(normBlock(body)), span = sp.map(s => SourceSpan(s.file, s.line, s.col)))
    case C.StructDef(name, fields, sp) =>
      Element(
        kind = "struct",
        name = Some(name),
        children = fields.map { case (n, t) => Element("field", name = Some(n), attrs = List(Attr("type", renderType(t)))) },
        span = sp.map(s => SourceSpan(s.file, s.line, s.col))
      )

  private def normBlock(b: C.Block): Element =
    Element("block", children = b.stmts.map(normStmt))

  private def normStmt(s: C.Stmt): Element = s match
    case C.LetStmt(name, annot, init, sp) =>
      Element(
        kind = "let",
        name = Some(name),
        attrs = annot.toList.map(a => Attr("type", renderType(a))),
        children = List(Element("init", children = List(normExpr(init)))),
        span = sp.map(s => SourceSpan(s.file, s.line, s.col))
      )
    case C.ReturnStmt(e, sp) => Element("return", children = List(normExpr(e)), span = sp.map(s => SourceSpan(s.file, s.line, s.col)))
    case C.IfStmt(c, t, e, sp) =>
      Element(
        kind = "if",
        children = List(
          Element("cond", children = List(normExpr(c))),
          normBlock(t),
          e.map(normBlock).getOrElse(Element("block"))
        ),
        span = sp.map(s => SourceSpan(s.file, s.line, s.col))
      )
    case C.WhileStmt(c, body, sp) =>
      Element(
        kind = "while",
        children = List(Element("cond", children = List(normExpr(c))), normBlock(body)),
        span = sp.map(s => SourceSpan(s.file, s.line, s.col))
      )
    case C.ForInStmt(name, it, body, sp) =>
      Element(
        kind = "for",
        attrs = List(Attr("var", name)),
        children = List(Element("iter", children = List(normExpr(it))), normBlock(body)),
        span = sp.map(s => SourceSpan(s.file, s.line, s.col))
      )
    case C.ExprStmt(e, sp) => e match
      case C.Binary("=", C.Var(n), rhs) =>
        Element("assign", name = Some(n), children = List(normExpr(rhs)), span = sp.map(s => SourceSpan(s.file, s.line, s.col)))
      case _ => Element("expr", children = List(normExpr(e)), span = sp.map(s => SourceSpan(s.file, s.line, s.col)))
    case C.MatchStmt(tgt, cases, sp) =>
      Element(
        kind = "match",
        children = Element("target", children = List(normExpr(tgt))) :: cases.map { c =>
          Element("case", children = List(normPattern(c.pattern), normBlock(c.body)))
        },
        span = sp.map(s => SourceSpan(s.file, s.line, s.col))
      )

  private def normExpr(e: C.Expr): Element = e match
    case C.IntLit(v)    => Element("int", attrs = List(Attr("value", v.toString)))
    case C.StrLit(s)    => Element("string", attrs = List(Attr("value", s)))
    case C.BoolLit(b)   => Element("bool", attrs = List(Attr("value", b.toString)))
    case C.Var(n)       => Element("var", name = Some(n))
    case C.Call(n, as)  => Element("call", name = Some(n), children = as.map(normExpr))
    case C.MethodCall(recv, n, as) =>
      Element("mcall", name = Some(n), children = normExpr(recv) :: as.map(normExpr))
    case C.Unary(op, x) => Element("call", name = Some(op), children = List(normExpr(x)))
    case C.Binary(op, l, r) => Element("call", name = Some(op), children = List(normExpr(l), normExpr(r)))
    case C.ListLit(es)  => Element("list", children = es.map(normExpr))
    case C.DictLit(ps)  => Element("dict", children = ps.map { case (k, v) => Element("pair", children = List(normExpr(k), normExpr(v))) })
    case C.Index(t, k)  => Element("index", children = List(Element("target", children = List(normExpr(t))), Element("key", children = List(normExpr(k)))))
    case C.Field(t, n)  => Element("field", attrs = List(Attr("name", n)), children = List(normExpr(t)))
    case C.Group(x)     => normExpr(x)
    case C.Lambda(ps, b) =>
      val paramsStr = ps.map(pp => s"${pp.name}:${renderType(pp.tpe)}").mkString(",")
      Element("lambda", attrs = List(Attr("params", paramsStr)), children = List(normBlock(b)))

  private def normPattern(p: C.Pattern): Element = p match
    case C.PInt(v)  => Element("pint", attrs = List(Attr("value", v.toString)))
    case C.PStr(s)  => Element("pstr", attrs = List(Attr("value", s)))
    case C.PBool(b) => Element("pbool", attrs = List(Attr("value", b.toString)))
    case C.PVar(n)  => Element("pvar", name = Some(n))
    case C.PWildcard => Element("pwild")

  private def renderType(t: C.TypeAnnot): String =
    if t.args.isEmpty then t.name else s"${t.name}[${t.args.map(renderType).mkString(",")}]"
