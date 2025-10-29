package com.github.kmizu.treep.types

import munit.FunSuite
import com.github.kmizu.treep.parser.Parser
import com.github.kmizu.treep.east.Normalize
import com.github.kmizu.treep.`macro`.Macro

class CheckerSpec extends FunSuite {
  test("well-typed add passes") {
    val src = "def add(x: Int, y: Int) returns: Int { return x + y }"
    val east = Macro.expand(Normalize.toEAST(Parser.parseProgram(src)))
    val diags = Checker.check(east)
    assertEquals(diags.isEmpty, true)
  }

  test("return type mismatch is reported") {
    val src = "def f() returns: Int { return \"x\" }"
    val east = Macro.expand(Normalize.toEAST(Parser.parseProgram(src)))
    val diags = Checker.check(east)
    assert(diags.nonEmpty)
  }

  test("list element type unify mismatch") {
    val src = "const a = [1, true]"
    val east = Normalize.toEAST(Parser.parseProgram(src))
    val diags = Checker.check(east)
    assert(diags.nonEmpty)
  }

  test("arity mismatch is reported") {
    val src = "def add(x: Int, y: Int) returns: Int { return x + y } def g() returns: Int { return add(1) }"
    val east = Macro.expand(Normalize.toEAST(Parser.parseProgram(src)))
    val diags = Checker.check(east)
    assert(diags.nonEmpty)
  }
}

