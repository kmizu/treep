package com.github.kmizu.treep.interpreter

import com.github.kmizu.treep.east.*
import com.github.kmizu.treep.east.ParamParser
import com.github.kmizu.treep.interpreter.*

import scala.collection.mutable

object Interpreter:
  sealed trait Value
  case object VUnit extends Value
  final case class VInt(v: Int) extends Value
  final case class VBool(v: Boolean) extends Value
  final case class VString(v: String) extends Value
  final case class VList(vs: List[Value]) extends Value
  final case class VDict(m: Map[Value, Value]) extends Value
  final case class VIter(private val data: List[Value], private var ix: Int) extends Value:
    def hasNext: Boolean = ix < data.length
    def next(): Value = { val r = data(ix); ix += 1; r }
    def remaining: List[Value] = data.drop(ix)
  final case class VTuple2(a: Value, b: Value) extends Value
  final case class VFunc(params: List[String], body: Element, env: Env) extends Value

  final case class Env(scopes: List[mutable.Map[String, Value]]):
    def push(): Env = Env(mutable.Map.empty[String, Value] :: scopes)
    def pop(): Env = Env(scopes.tail)
    def set(name: String, v: Value): Unit = scopes.head.update(name, v)
    def get(name: String): Option[Value] = scopes.collectFirst(Function.unlift(_.get(name)))
    def assign(name: String, v: Value): Unit =
      val target = scopes.reverse.find(_.contains(name)).getOrElse(scopes.head)
      target.update(name, v)

  object Env:
    def empty: Env = Env(List(mutable.Map.empty[String, Value]))

  final case class Return(v: Value) extends Throwable

  def run(program: Element): Unit =
    val env = loadProgram(program)
    // call main() if present
    env.get("main") match
      case Some(fn: VFunc) if fn.params.isEmpty =>
        try
          val res = callFunc(fn, Nil)
          res match
            case VInt(n) => println(s"[treep] exit ${n}")
            case VUnit   => println("[treep] ok")
            case _       => println("[treep] ok")
        catch
          case _: Return => println("[treep] returned unexpectedly")
      case _ => println("[treep] ok")

  def evalFunction(program: Element, name: String, args: List[Value] = Nil): Value =
    val env = loadProgram(program)
    env.get(name) match
      case Some(fn: VFunc) => callFunc(fn, args)
      case _ => throw UndefinedFunction(name)

  private def formatValue(v: Value): String = v match
    case VUnit         => "()"
    case VInt(n)       => n.toString
    case VBool(b)      => b.toString
    case VString(s)    => s
    case VList(xs)     => xs.map(formatValue).mkString("[", ", ", "]")
    case VDict(m) =>
      val entries = m.toList.map { case (k, value) => s"${formatValue(k)}: ${formatValue(value)}" }
      entries.mkString("{", ", ", "}")
    case vIter: VIter  => s"<iter ${vIter.remaining.length} remaining>"
    case VTuple2(a, b) => s"(${formatValue(a)}, ${formatValue(b)})"
    case VFunc(_, _, _) => "<function>"

  private def loadProgram(program: Element): Env =
    var env = Env.empty
    def evalTop(el: Element): Unit = el.kind match
      case "def" =>
        val fname = el.name.getOrElse(throw MissingAttribute("name", "function definition"))
        val params: List[String] =
          el.getAttr("params").filter(_.nonEmpty)
            .map(ParamParser.parseParamNames).getOrElse(Nil)
        env.set(fname, VFunc(params, el.children.headOption.getOrElse(Element("block")), env))
      case "const" =>
        val cname = el.name.getOrElse(throw MissingAttribute("name", "const declaration"))
        val init = el.getChild("init").getOrElse(Element("unit"))
        val v = evalExpr(env, init)
        env.set(cname, v)
      case "module" => el.children.foreach(evalTop)
      case "struct" => () // ignore for now
      case other => ()
    program.children.foreach(evalTop)
    env

  private def callFunc(fn: VFunc, args: List[Value]): Value =
    if fn.params.length != args.length then
      throw ArityMismatch(fn.params.length, args.length)
    var callEnv = Env(mutable.Map.from(fn.env.scopes.head) :: fn.env.scopes.tail) // shallow copy head
    callEnv = callEnv.push()
    fn.params.zip(args).foreach { case (n, v) => callEnv.set(n, v) }
    try
      evalBlock(callEnv, fn.body)
      VUnit
    catch
      case Return(v) => v

  private def evalBlock(env0: Env, block: Element): Unit =
    var env = env0.push()
    block.children.foreach { st => evalStmt(env, st) }
    ()

  // Evaluate statements without pushing a new scope (used for loop bodies)
  private def evalBlockInPlace(env: Env, block: Element): Unit =
    block.children.foreach { st => evalStmt(env, st) }

  private def evalStmt(env: Env, st: Element): Unit = st.kind match
    case "let" =>
      val name = st.name.getOrElse(throw MissingAttribute("name", "let statement"))
      val init = st.getChild("init").getOrElse(throw MissingElement("init", "let statement"))
      val v = evalExpr(env, init)
      env.set(name, v)
    case "assign" =>
      val name = st.name.getOrElse(throw MissingAttribute("name", "assignment"))
      val rhs = st.children.headOption.getOrElse(throw MissingElement("rhs", "assignment"))
      val v = evalExpr(env, rhs)
      env.assign(name, v)
    case "return" =>
      val v = st.children.headOption.map(evalExpr(env, _)).getOrElse(VUnit)
      throw Return(v)
    case "block" => evalBlock(env, st)
    case "expr" =>
      st.children.headOption.foreach(evalExpr(env, _))
    case "if" =>
      val cond = st.getChild("cond").getOrElse(throw MissingElement("cond", "if statement"))
      val c = asBool(evalExpr(env, cond))
      val others = st.children.filter(_.kind == "block")
      val thenBlk = others.headOption.getOrElse(Element("block"))
      val elseBlk = if others.size > 1 then Some(others(1)) else None
      if c then evalBlock(env, thenBlk) else elseBlk.foreach(evalBlock(env, _))
    case "while" =>
      val cond = st.getChild("cond").getOrElse(throw MissingElement("cond", "while loop"))
      val body = st.children.find(_.kind == "block").getOrElse(Element("block"))
      while asBool(evalExpr(env, cond)) do
        evalBlockInPlace(env, body)
    case "match" =>
      val tgt = st.getChild("target").getOrElse(throw MissingElement("target", "match statement"))
      val v = evalExpr(env, tgt)
      val cases = st.children.filter(_.kind == "case")

      val hit = cases.collectFirst { c =>
        matchPattern(env, c.children.head, v).map { binds => (c.children(1), binds) }
      }.flatten
      val chosen = hit.orElse {
        cases.find(_.children.headOption.exists(_.kind.trim == "pwild")).map(c => (c.children(1), Map.empty[String, Value]))
      }
      chosen match
        case Some((body, binds)) =>
          val env2 = env.push(); binds.foreach { case (n, vv) => env2.set(n, vv) }
          evalBlock(env2, body)
        case None => ()
    case _ => ()

  private def evalExpr(env: Env, e: Element): Value = e.kind match
    case "unit"   => VUnit
    case "int"    => VInt(e.getAttrOrElse("value", "0").toInt)
    case "bool"   => VBool(e.getAttr("value").contains("true"))
    case "string" => VString(e.getAttrOrElse("value", ""))
    case "var"    =>
      val varName = e.name.getOrElse(throw MissingAttribute("name", "variable reference"))
      env.get(varName).getOrElse(throw UndefinedVariable(varName))
    case "list"   => VList(e.children.map(ch => evalExpr(env, ch)))
    case "dict"   =>
      val pairs: Map[Value, Value] = e.children.map { p =>
        val k = evalExpr(env, p.children.head)
        val v = evalExpr(env, p.children(1))
        k -> v
      }.toMap
      VDict(pairs)
    case "index"  =>
      val t = e.getChild("target").getOrElse(throw MissingElement("target", "index expression"))
      val k = e.getChild("key").getOrElse(throw MissingElement("key", "index expression"))
      (evalExpr(env, t), evalExpr(env, k)) match
        case (VDict(m), key) => m.getOrElse(key, VUnit)
        case (VList(xs), VInt(i)) => xs.lift(i).getOrElse(VUnit)
        case (recvType, _) => throw IndexError(recvType.getClass.getSimpleName, "not indexable")
    case "field" =>
      val t = e.children.headOption.getOrElse(throw MissingElement("target", "field access"))
      val n = e.getAttr("name").getOrElse(throw MissingAttribute("name", "field access"))
      evalExpr(env, Element("index", children = List(Element("target", children = List(t)), Element("key", children = List(Element("string", attrs = List(Attr("value", n))))))))
    case "call"   =>
      val name = e.name.getOrElse(throw MissingAttribute("name", "function call"))
      if name == "=" then
        // assignment: left must be var; evaluate rhs then assign
        val lhs = e.children.headOption.getOrElse(throw MissingElement("lhs", "assignment"))
        val rhs = e.children.lift(1).getOrElse(throw MissingElement("rhs", "assignment"))
        val rhsV = evalExpr(env, rhs)
        lhs.kind match
          case "var" =>
            val varName = lhs.name.getOrElse(throw MissingAttribute("name", "assignment target"))
            env.assign(varName, rhsV)
            rhsV
          case _ => throw AssignmentError("assignment target must be a variable")
      else if name == "&&" then
        val l = evalExpr(env, e.children.head)
        if !asBool(l) then VBool(false) else VBool(asBool(evalExpr(env, e.children(1))))
      else if name == "||" then
        val l = evalExpr(env, e.children.head)
        if asBool(l) then VBool(true) else VBool(asBool(evalExpr(env, e.children(1))))
      else
        val args = e.children.map(ch => evalExpr(env, ch))
        evalCall(env, name, args)
    case "mcall" =>
      val name = e.name.getOrElse(throw MissingAttribute("name", "method call"))
      val recv = e.children.headOption.map(evalExpr(env, _)).getOrElse(throw MissingElement("receiver", "method call"))
      val args = e.children.tail.map(ch => evalExpr(env, ch))
      (name, recv, args) match
        // String methods
        case ("split", VString(s), List(VString(delimiter))) =>
          VList(s.split(java.util.regex.Pattern.quote(delimiter), -1).toList.map(VString(_)))
        case ("length", VString(s), Nil) => VInt(s.length)
        case ("substring", VString(s), List(VInt(start), VInt(end))) =>
          val validStart = math.max(0, math.min(start, s.length))
          val validEnd = math.max(validStart, math.min(end, s.length))
          VString(s.substring(validStart, validEnd))
        case ("contains", VString(s), List(VString(substr))) => VBool(s.contains(substr))
        // List methods
        case ("push", VList(xs), List(v)) => VList(xs :+ v)
        case ("iter", VList(xs), Nil) => VIter(xs, 0)
        case ("length", VList(xs), Nil) => VInt(xs.length)
        case ("head", VList(xs), Nil) => xs.headOption.getOrElse(VUnit)
        case ("tail", VList(xs), Nil) => VList(if xs.nonEmpty then xs.tail else Nil)
        case ("append", VList(xs), List(v)) => VList(xs :+ v)
        case ("concat", VList(xs), List(VList(ys))) => VList(xs ++ ys)
        case ("join", VList(xs), List(VString(delimiter))) =>
          VString(xs.map(formatValue).mkString(delimiter))
        // Dict methods
        case ("hasKey", VDict(m), List(k)) => VBool(m.contains(k))
        case ("keys", VDict(m), Nil) => VList(m.keys.toList)
        case ("get", VDict(m), List(k)) => m.getOrElse(k, VUnit)
        case ("iter", VDict(m), Nil) => VIter(m.toList.map { case (k, v) => VTuple2(k, v) }, 0)
        case ("size", VDict(m), Nil) => VInt(m.size)
        case ("values", VDict(m), Nil) => VList(m.values.toList)
        case ("entries", VDict(m), Nil) => VList(m.toList.map { case (k, v) => VTuple2(k, v) })
        case ("put", VDict(m), List(k, v)) => VDict(m.updated(k, v))
        case ("remove", VDict(m), List(k)) => VDict(m - k)
        case ("getOrElse", VDict(m), List(k, d)) => m.getOrElse(k, d)
        // Iter methods
        case ("hasNext", it: VIter, Nil) => VBool(it.hasNext)
        case ("next", it: VIter, Nil) => it.next()
        case ("toList", it: VIter, Nil) => VList(it.remaining)
        case _ =>
          // Record-style method: field function under string key
          recv match
            case VDict(m) =>
              m.get(VString(name)) match
                case Some(fn: VFunc) =>
                  val arity = fn.params.length
                  if arity == args.length + 1 then callFunc(fn, recv :: args)
                  else if arity == args.length then callFunc(fn, args)
                  else throw ArityMismatch(arity, args.length, Some(s"method $name"))
                case _ =>
                  // Fallback: treat as function(name) with (recv :: args)
                  env.get(name) match
                    case Some(fn: VFunc) => callFunc(fn, recv :: args)
                    case _ => throw InvalidOperation(s"method call $name", "method not found")
            case _ =>
          // Fallback: treat as function(name) with (recv :: args)
              env.get(name) match
                case Some(fn: VFunc) => callFunc(fn, recv :: args)
                case _ => throw InvalidOperation(s"method call $name", "method not found")
    case "lambda" =>
      val body = e.children.headOption.getOrElse(Element("block"))
      val params = e.getAttr("params")
        .map(ParamParser.parseParamNames)
        .orElse(e.getAttr("param").map(a => List(a)))
        .getOrElse(Nil)
      VFunc(params, body, env)
    case other => VUnit

  // Arithmetic operators
  private def evalArithmeticOp(name: String, args: List[Value]): Value = name match
    case "+" => args match
      case List(VInt(a), VInt(b)) => VInt(a + b)
      case List(VString(a), VString(b)) => VString(a + b)
      case _ => throw TypeMismatchError("(Int, Int) or (String, String)", args.toString, Some("+"))
    case "-" => args match
      case List(VInt(a), VInt(b)) => VInt(a - b)
      case _ => throw TypeMismatchError("(Int, Int)", args.toString, Some("-"))
    case "*" => args match
      case List(VInt(a), VInt(b)) => VInt(a * b)
      case _ => throw TypeMismatchError("(Int, Int)", args.toString, Some("*"))
    case "/" => args match
      case List(VInt(a), VInt(0)) => throw DivisionByZero()
      case List(VInt(a), VInt(b)) => VInt(a / b)
      case _ => throw TypeMismatchError("(Int, Int)", args.toString, Some("/"))
    case "%" => args match
      case List(VInt(a), VInt(0)) => throw DivisionByZero()
      case List(VInt(a), VInt(b)) => VInt(a % b)
      case _ => throw TypeMismatchError("(Int, Int)", args.toString, Some("%"))

  // Comparison and logical operators
  private def evalComparisonOp(name: String, args: List[Value]): Value = name match
    case "==" => args match
      case List(a, b) => VBool(eqValue(a, b))
      case _ => throw ArityMismatch(2, args.length, Some("=="))
    case "!=" => args match
      case List(a, b) => VBool(!eqValue(a, b))
      case _ => throw ArityMismatch(2, args.length, Some("!="))
    case ">"  => args match
      case List(VInt(a), VInt(b)) => VBool(a > b)
      case _ => throw TypeMismatchError("(Int, Int)", args.toString, Some(">"))
    case ">=" => args match
      case List(VInt(a), VInt(b)) => VBool(a >= b)
      case _ => throw TypeMismatchError("(Int, Int)", args.toString, Some(">="))
    case "<"  => args match
      case List(VInt(a), VInt(b)) => VBool(a < b)
      case _ => throw TypeMismatchError("(Int, Int)", args.toString, Some("<"))
    case "<=" => args match
      case List(VInt(a), VInt(b)) => VBool(a <= b)
      case _ => throw TypeMismatchError("(Int, Int)", args.toString, Some("<="))
    case "&&" => args match
      case List(VBool(a), VBool(b)) => VBool(a && b)
      case _ => throw TypeMismatchError("(Bool, Bool)", args.toString, Some("&&"))
    case "||" => args match
      case List(VBool(a), VBool(b)) => VBool(a || b)
      case _ => throw TypeMismatchError("(Bool, Bool)", args.toString, Some("||"))

  // Iterator functions
  private def evalIteratorFunc(name: String, args: List[Value]): Value = name match
    case "iter" => args match
      case List(VList(xs)) => VIter(xs, 0)
      case List(VDict(m)) => VIter(m.toList.map { case (k, v) => VTuple2(k, v) }, 0)
      case _ => throw TypeMismatchError("List or Dict", args.toString, Some("iter"))
    case "hasNext" => args match
      case List(it: VIter) => VBool(it.hasNext)
      case _ => VBool(false)
    case "next" => args match
      case List(it: VIter) => it.next()
      case _ => throw TypeMismatchError("Iterator", args.toString, Some("next"))

  // IO functions
  private def evalIOFunc(name: String, args: List[Value]): Value = name match
    case "print" => args match
      case List(value) =>
        scala.Predef.print(formatValue(value))
        VUnit
      case _ => throw ArityMismatch(1, args.length, Some("print"))
    case "println" => args match
      case List(value) =>
        scala.Predef.println(formatValue(value))
        VUnit
      case _ => throw ArityMismatch(1, args.length, Some("println"))

  // Collection helper functions
  private def evalCollectionFunc(name: String, args: List[Value]): Value = name match
    case "push" => args match
      case List(VList(xs), v) => VList(xs :+ v)
      case _ => throw TypeMismatchError("(List, value)", args.toString, Some("push"))
    case "keys" => args match
      case List(VDict(m)) => VList(m.keys.toList)
      case _ => throw TypeMismatchError("Dict", args.toString, Some("keys"))
    case "hasKey" => args match
      case List(VDict(m), k) => VBool(m.contains(k))
      case _ => VBool(false)
    case "fst" => args match
      case List(VTuple2(a, _)) => a
      case _ => throw TypeMismatchError("Tuple2", args.toString, Some("fst"))
    case "snd" => args match
      case List(VTuple2(_, b)) => b
      case _ => throw TypeMismatchError("Tuple2", args.toString, Some("snd"))

  // Math functions
  private def evalMathFunc(name: String, args: List[Value]): Value = name match
    case "abs" => args match
      case List(VInt(n)) => VInt(math.abs(n))
      case _ => throw TypeMismatchError("Int", args.toString, Some("abs"))
    case "min" => args match
      case List(VInt(a), VInt(b)) => VInt(math.min(a, b))
      case _ => throw TypeMismatchError("(Int, Int)", args.toString, Some("min"))
    case "max" => args match
      case List(VInt(a), VInt(b)) => VInt(math.max(a, b))
      case _ => throw TypeMismatchError("(Int, Int)", args.toString, Some("max"))
    case "pow" => args match
      case List(VInt(base), VInt(exp)) =>
        if exp < 0 then throw InvalidOperation("pow", "negative exponent not supported for integers")
        else VInt(math.pow(base.toDouble, exp.toDouble).toInt)
      case _ => throw TypeMismatchError("(Int, Int)", args.toString, Some("pow"))

  private def evalCall(env: Env, name: String, args: List[Value]): Value =
    // Try each category of operators
    if Set("+", "-", "*", "/", "%").contains(name) then
      evalArithmeticOp(name, args)
    else if Set("==", "!=", ">", ">=", "<", "<=", "&&", "||").contains(name) then
      evalComparisonOp(name, args)
    else if Set("iter", "hasNext", "next").contains(name) then
      evalIteratorFunc(name, args)
    else if Set("print", "println").contains(name) then
      evalIOFunc(name, args)
    else if Set("push", "keys", "hasKey", "fst", "snd").contains(name) then
      evalCollectionFunc(name, args)
    else if Set("abs", "min", "max", "pow").contains(name) then
      evalMathFunc(name, args)
    else
      // user-defined functions
      env.get(name) match
        case Some(fn: VFunc) => callFunc(fn, args)
        case _ => throw UndefinedFunction(name)

  private def asBool(v: Value): Boolean = v match
    case VBool(b) => b
    case VInt(n)  => n != 0
    case VUnit    => false
    case _        => true

  private def eqValue(a: Value, b: Value): Boolean = (a, b) match
    case (VInt(x), VInt(y)) => x == y
    case (VBool(x), VBool(y)) => x == y
    case (VString(x), VString(y)) => x == y
    case (VList(xs), VList(ys)) => xs.length == ys.length && xs.zip(ys).forall(eqValue.tupled)
    case (VDict(mx), VDict(my)) => mx.keySet == my.keySet && mx.forall { case (k, vx) => my.get(k).exists(eqValue(vx, _)) }
    case (VUnit, VUnit) => true
    case _ => false

  private def matchPattern(env: Env, pat: Element, v: Value): Option[Map[String, Value]] = pat.kind match
    case "pint"  =>
      val expected = pat.getAttrOrElse("value", "0")
      Some(Map.empty).filter(_ => v match { case VInt(n) => n.toString == expected; case _ => false })
    case "pstr"  =>
      val expected = pat.getAttrOrElse("value", "")
      Some(Map.empty).filter(_ => v match { case VString(s) => s == expected; case _ => false })
    case "pbool" =>
      val expected = pat.getAttrOrElse("value", "false")
      Some(Map.empty).filter(_ => v match { case VBool(b) => b.toString == expected; case _ => false })
    case "pvar"  =>
      val varName = pat.name.getOrElse(throw MissingAttribute("name", "pattern variable"))
      Some(Map(varName -> v))
    case "pwild" => Some(Map.empty)
    case _        => None
