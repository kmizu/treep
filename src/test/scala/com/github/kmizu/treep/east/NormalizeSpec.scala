package com.github.kmizu.treep.east

import munit.FunSuite
import com.github.kmizu.treep.parser.Parser

class NormalizeSpec extends FunSuite {

  test("normalize def with params and returns + block") {
    val src = "def add(x: Int, y: Int) returns: Int { return x + y }"
    val east = Normalize.toEAST(Parser.parseProgram(src))
    val defs = east.children.filter(_.kind == "def")
    assertEquals(defs.length, 1)
    val d = defs.head
    assertEquals(d.name, Some("add"))
    assert(d.attrs.exists(_.key == "params"))
    assert(d.attrs.exists(a => a.key == "returns" && a.value == "Int"))
    assert(d.children.exists(_.kind == "block"))
  }

  test("normalize const and let with init expr") {
    val src = "const a: Int = 1\nmodule M { let b = a }"
    val east = Normalize.toEAST(Parser.parseProgram(src))
    val const = east.children.find(_.kind == "const").get
    assertEquals(const.name, Some("a"))
    assert(const.attrs.exists(a => a.key == "type" && a.value == "Int"))
    assert(const.children.exists(_.kind == "init"))
  }

  test("normalize if with cond/then/else and calls for operators") {
    val src = "def f() returns: Int { if (1 + 2 * 3 > 0) { return 1 } else { return 0 } }"
    val east = Normalize.toEAST(Parser.parseProgram(src))
    val f = east.children.find(e => e.kind == "def" && e.name.contains("f")).get
    val block = f.children.head
    val ifs = block.children.find(_.kind == "if").get
    val cond = ifs.children.head
    // inside cond, expect nested calls; just assert the outermost is a call
    assertEquals(cond.kind, "cond")
    val call = cond.children.head
    assertEquals(call.kind, "call")
  }

  test("normalize list, dict, field, index") {
    val src = "const xs = [1,2]\nconst m = { \"a\": 1 }\nconst v = m[\"a\"]"
    val east = Normalize.toEAST(Parser.parseProgram(src))
    val kinds = east.children.map(_.kind).toSet
    assert(kinds.contains("const"))
  }

  test("normalize struct with fields") {
    val src = "struct Point { x: Int, y: Int }"
    val east = Normalize.toEAST(Parser.parseProgram(src))
    val st = east.children.find(_.kind == "struct").get
    assertEquals(st.name, Some("Point"))
    assert(st.children.exists(f => f.kind == "field" && f.name.contains("x")))
  }

  test("normalize match with cases and patterns") {
    val src = "def f(n: Int) returns: Int { match (n) { case 0 => { return 1 } case _ => { return n } } }"
    val east = Normalize.toEAST(Parser.parseProgram(src))
    val f = east.children.find(e => e.kind == "def" && e.name.contains("f")).get
    val block = f.children.head
    val m = block.children.find(_.kind == "match").get
    assert(m.children.exists(_.kind == "target"))
    assert(m.children.exists(_.kind == "case"))
  }

  test("normalize for-in with binder attr and iter child") {
    val src = "def g() returns: Unit { for (x in: [1,2,3]) { return 0 } }"
    val east = Normalize.toEAST(Parser.parseProgram(src))
    val d = east.children.find(e => e.kind == "def" && e.name.contains("g")).get
    val block = d.children.head
    val fr = block.children.find(_.kind == "for").get
    assert(fr.attrs.exists(a => a.key == "var" && a.value == "x"))
    val iter = fr.children.head
    assertEquals(iter.kind, "iter")
  }

  test("normalize assignment statement from binary '='") {
    val src = "def a() returns: Int { let acc = [] acc = push(acc, 1) return acc[0] }"
    val east = Normalize.toEAST(Parser.parseProgram(src))
    val d = east.children.find(e => e.kind == "def" && e.name.contains("a")).get
    val block = d.children.head
    assert(block.children.exists(_.kind == "assign"))
  }
}

