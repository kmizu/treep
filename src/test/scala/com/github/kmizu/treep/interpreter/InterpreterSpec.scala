package com.github.kmizu.treep.interpreter

import munit.FunSuite
import com.github.kmizu.treep.parser.Parser
import com.github.kmizu.treep.east.Normalize
import com.github.kmizu.treep.interpreter.Interpreter.*
import com.github.kmizu.treep.`macro`.Macro

class InterpreterSpec extends FunSuite {

  private def valueOfInt(v: Value): Int = v match
    case VInt(n) => n
    case other => fail(s"expected int, got ${other}")

  test("for-in early return yields first element") {
    val src = "def f() returns: Int { for (x in: [1,2]) { return x } return 0 }"
    val east = Macro.expand(Normalize.toEAST(Parser.parseProgram(src)))
    val res = Interpreter.evalFunction(east, "f")
    assertEquals(valueOfInt(res), 1)
  }

  test("assignment with push builds list") {
    val src =
      """
      |def h() returns: Int {
      |  let acc = []
      |  for (x in: [1,2,3]) { acc = acc.push(x) }
      |  return acc[2]
      |}
      |""".stripMargin
    val east = Macro.expand(Normalize.toEAST(Parser.parseProgram(src)))
    val res = Interpreter.evalFunction(east, "h")
    assertEquals(valueOfInt(res), 3)
  }

  test("if/while/arithmetic nesting") {
    val src =
      """
      |def g() returns: Int {
      |  if (1 + 2 * 3 > 0) { return 42 } else { return 0 }
      |}
      |""".stripMargin
    val east = Macro.expand(Normalize.toEAST(Parser.parseProgram(src)))
    val res = Interpreter.evalFunction(east, "g")
    assertEquals(valueOfInt(res), 42)
  }

  test("dict index via field and index") {
    val src =
      """
      |const m = { "a": 7 }
      |def h() returns: Int { return m["a"] }
      |""".stripMargin
    val east = Macro.expand(Normalize.toEAST(Parser.parseProgram(src)))
    val res = Interpreter.evalFunction(east, "h")
    assertEquals(valueOfInt(res), 7)
  }

  test("short-circuit avoids evaluating rhs") {
    val src =
      """
      |def s1() returns: Int { if (false && unknown()) { return 0 } else { return 7 } }
      |def s2() returns: Int { if (true || unknown()) { return 9 } else { return 0 } }
      |""".stripMargin
    val east = Macro.expand(Normalize.toEAST(Parser.parseProgram(src)))
    val r1 = Interpreter.evalFunction(east, "s1")
    val r2 = Interpreter.evalFunction(east, "s2")
    assertEquals(valueOfInt(r1), 7)
    assertEquals(valueOfInt(r2), 9)
  }

  test("iterate dict via keys and sum") {
    val src =
      """
      |const m = { "a": 1, "b": 2 }
      |def sumk() returns: Int {
      |  let s = 0
      |  for (k in: keys(m)) { s = s + m[k] }
      |  return s
      |}
      |""".stripMargin
    val east = Macro.expand(Normalize.toEAST(Parser.parseProgram(src)))
    val rk = Interpreter.evalFunction(east, "sumk")
    assertEquals(valueOfInt(rk), 3)
  }

  test("iterate dict pairs and sum values via snd") {
    val src =
      """
      |const m = { "a": 1, "b": 2 }
      |def sumv() returns: Int {
      |  let s = 0
      |  for (p in: m) { s = s + snd(p) }
      |  return s
      |}
      |""".stripMargin
    val east = Macro.expand(Normalize.toEAST(Parser.parseProgram(src)))
    val rv = Interpreter.evalFunction(east, "sumv")
    assertEquals(valueOfInt(rv), 3)
  }

  test("match statement basic") {
    val src =
      """
      |def m(n: Int) returns: Int { match (n) { case 0 => { return 1 } case _ => { return n } } }
      |""".stripMargin
    val east = Macro.expand(Normalize.toEAST(Parser.parseProgram(src)))
    val r0 = Interpreter.evalFunction(east, "m", Nil :+ VInt(0))
    val r3 = Interpreter.evalFunction(east, "m", Nil :+ VInt(3))
    assertEquals(valueOfInt(r0), 1)
    assertEquals(valueOfInt(r3), 3)
  }
}

