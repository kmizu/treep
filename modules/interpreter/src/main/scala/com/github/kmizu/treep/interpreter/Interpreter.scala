package com.github.kmizu.treep.interpreter

import com.github.kmizu.treep.east.*

import scala.collection.mutable

object Interpreter:
  sealed trait Value
  case object VUnit extends Value
  final case class VInt(v: Int) extends Value
  final case class VBool(v: Boolean) extends Value
  final case class VString(v: String) extends Value
  final case class VList(vs: List[Value]) extends Value
  final case class VDict(m: Map[String, Value]) extends Value
  final case class VIter(private val data: List[Value], private var ix: Int) extends Value:
    def hasNext: Boolean = ix < data.length
    def next(): Value = { val r = data(ix); ix += 1; r }
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
      case other => throw new RuntimeException(s"function not found: ${name}, got ${other}")

  private def loadProgram(program: Element): Env =
    var env = Env.empty
    def evalTop(el: Element): Unit = el.kind match
      case "def" =>
        val fname = el.name.getOrElse(throw new RuntimeException("anonymous def"))
        val params: List[String] =
          el.attrs.find(_.key == "params").map(_.value).filter(_.nonEmpty)
            .map(_.split(",").toList.map(_.trim.split(":")(0).trim)).getOrElse(Nil)
        env.set(fname, VFunc(params, el.children.headOption.getOrElse(Element("block")), env))
      case "const" =>
        val cname = el.name.get
        val init = el.children.find(_.kind == "init").flatMap(_.children.headOption).getOrElse(Element("unit"))
        val v = evalExpr(env, init)
        env.set(cname, v)
      case "module" => el.children.foreach(evalTop)
      case "struct" => () // ignore for now
      case other => ()
    program.children.foreach(evalTop)
    env

  private def callFunc(fn: VFunc, args: List[Value]): Value =
    if fn.params.length != args.length then
      throw new RuntimeException(s"arity mismatch: expected ${fn.params.length}, found ${args.length}")
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

  private def evalStmt(env: Env, st: Element): Unit = st.kind match
    case "let" =>
      val name = st.name.get
      val init = st.children.find(_.kind == "init").flatMap(_.children.headOption).get
      val v = evalExpr(env, init)
      env.set(name, v)
    case "assign" =>
      val name = st.name.get
      val rhs = st.children.head
      val v = evalExpr(env, rhs)
      env.assign(name, v)
    case "return" =>
      val v = st.children.headOption.map(evalExpr(env, _)).getOrElse(VUnit)
      throw Return(v)
    case "block" => evalBlock(env, st)
    case "if" =>
      val cond = st.children.find(_.kind == "cond").flatMap(_.children.headOption).get
      val c = asBool(evalExpr(env, cond))
      val t = st.children.find(_.kind == "block").getOrElse(Element("block"))
      val others = st.children.filter(_.kind == "block")
      val thenBlk = others.headOption.getOrElse(Element("block"))
      val elseBlk = if others.size > 1 then Some(others(1)) else None
      if c then evalBlock(env, thenBlk) else elseBlk.foreach(evalBlock(env, _))
    case "while" =>
      val cond = st.children.find(_.kind == "cond").flatMap(_.children.headOption).get
      val body = st.children.find(_.kind == "block").getOrElse(Element("block"))
      while asBool(evalExpr(env, cond)) do evalBlock(env, body)
    case "expr" => st.children.headOption.foreach(e => evalExpr(env, e))
    case "match" =>
      val tgtE = st.children.find(_.kind == "target").flatMap(_.children.headOption).get
      val v = evalExpr(env, tgtE)
      val cases = st.children.filter(_.kind == "case")
      var done = false
      cases.iterator.foreach { c =>
        if !done then
          val pat = c.children.head
          val body = c.children(1)
          matchPattern(env, pat, v) match
            case Some(bindings) =>
              val caseEnv = env.push()
              bindings.foreach { case (n, vv) => caseEnv.set(n, vv) }
              evalBlock(caseEnv, body)
              done = true
            case None => ()
      }
    case other => ()

  private def evalExpr(env: Env, e: Element): Value = e.kind match
    case "int"    => VInt(e.attrs.find(_.key == "value").get.value.toInt)
    case "string" => VString(e.attrs.find(_.key == "value").get.value)
    case "bool"   => VBool(e.attrs.find(_.key == "value").get.value.toBoolean)
    case "var"    => env.get(e.name.get).getOrElse(throw new RuntimeException(s"unbound: ${e.name.get}"))
    case "list"   => VList(e.children.map(ch => evalExpr(env, ch)))
    case "dict"   =>
      val pairs = e.children.map { p =>
        val k = p.attrs.find(_.key == "key").get.value
        val v = evalExpr(env, p.children.head)
        k -> v
      }.toMap
      VDict(pairs)
    case "index"  =>
      val t = e.children.find(_.kind == "target").flatMap(_.children.headOption).get
      val k = e.children.find(_.kind == "key").flatMap(_.children.headOption).get
      (evalExpr(env, t), evalExpr(env, k)) match
        case (VDict(m), VString(s)) => m.getOrElse(s, VUnit)
        case (VList(xs), VInt(i)) => xs.lift(i).getOrElse(VUnit)
        case other => throw new RuntimeException(s"index not supported: ${other}")
    case "field" =>
      val t = e.children.head
      val n = e.attrs.find(_.key == "name").get.value
      evalExpr(env, Element("index", children = List(Element("target", children = List(t)), Element("key", children = List(Element("string", attrs = List(Attr("value", n))))))))
    case "call"   =>
      val name = e.name.get
      if name == "=" then
        // assignment: left must be var; evaluate rhs then assign
        val lhs = e.children.head
        val rhsV = evalExpr(env, e.children(1))
        lhs.kind match
          case "var" => env.assign(lhs.name.get, rhsV); rhsV
          case _ => throw new RuntimeException("assignment target must be variable")
      else if name == "&&" then
        val l = evalExpr(env, e.children.head)
        if !asBool(l) then VBool(false) else VBool(asBool(evalExpr(env, e.children(1))))
      else if name == "||" then
        val l = evalExpr(env, e.children.head)
        if asBool(l) then VBool(true) else VBool(asBool(evalExpr(env, e.children(1))))
      else
        val args = e.children.map(ch => evalExpr(env, ch))
        evalCall(env, name, args)
    case other => VUnit

  private def evalCall(env: Env, name: String, args: List[Value]): Value = name match
    // arithmetic
    case "+" => (args: @unchecked) match { case List(VInt(a), VInt(b)) => VInt(a + b) }
    case "-" => (args: @unchecked) match { case List(VInt(a), VInt(b)) => VInt(a - b) }
    case "*" => (args: @unchecked) match { case List(VInt(a), VInt(b)) => VInt(a * b) }
    case "/" => (args: @unchecked) match { case List(VInt(a), VInt(b)) => VInt(a / b) }
    case "%" => (args: @unchecked) match { case List(VInt(a), VInt(b)) => VInt(a % b) }
    // comparison
    case "==" => (args: @unchecked) match { case List(a, b) => VBool(eqValue(a, b)) }
    case "!=" => (args: @unchecked) match { case List(a, b) => VBool(!eqValue(a, b)) }
    case ">"  => (args: @unchecked) match { case List(VInt(a), VInt(b)) => VBool(a > b) }
    case ">=" => (args: @unchecked) match { case List(VInt(a), VInt(b)) => VBool(a >= b) }
    case "<"  => (args: @unchecked) match { case List(VInt(a), VInt(b)) => VBool(a < b) }
    case "<=" => (args: @unchecked) match { case List(VInt(a), VInt(b)) => VBool(a <= b) }
    case "&&" => (args: @unchecked) match { case List(VBool(a), VBool(b)) => VBool(a && b) }
    case "||" => (args: @unchecked) match { case List(VBool(a), VBool(b)) => VBool(a || b) }
    // iterators
    case "iter" => args match
      case List(VList(xs)) => VIter(xs, 0)
      case List(VDict(m)) => VIter(m.values.toList, 0)
      case other => throw new RuntimeException(s"iter expects list/dict, got ${other}")
    case "hasNext" => args match
      case List(it: VIter) => VBool(it.hasNext)
      case _ => VBool(false)
    case "next" => args match
      case List(it: VIter) => it.next()
      case other => throw new RuntimeException(s"next expects iterator, got ${other}")
    // collection helpers
    case "push" => args match
      case List(VList(xs), v) => VList(xs :+ v)
      case other => throw new RuntimeException(s"push expects (List, any), got ${other}")
    case "keys" => args match
      case List(VDict(m)) => VList(m.keys.toList.sorted.map(VString.apply))
      case _ => throw new RuntimeException("keys expects Dict")
    case "hasKey" => args match
      case List(VDict(m), VString(k)) => VBool(m.contains(k))
      case _ => VBool(false)
    // user-defined functions
    case other =>
      env.get(other) match
        case Some(fn: VFunc) => callFunc(fn, args)
        case _ => throw new RuntimeException(s"unknown call: ${other}")

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
    case "pint"  => Some(Map.empty).filter(_ => v match { case VInt(n) => n.toString == pat.attrs.find(_.key == "value").get.value; case _ => false })
    case "pstr"  => Some(Map.empty).filter(_ => v match { case VString(s) => s == pat.attrs.find(_.key == "value").get.value; case _ => false })
    case "pbool" => Some(Map.empty).filter(_ => v match { case VBool(b) => b.toString == pat.attrs.find(_.key == "value").get.value; case _ => false })
    case "pvar"  => Some(Map(pat.name.get -> v))
    case "pwild" => Some(Map.empty)
    case _        => None
