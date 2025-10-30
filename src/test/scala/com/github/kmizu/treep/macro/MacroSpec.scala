package com.github.kmizu.treep.`macro`

import munit.FunSuite
import com.github.kmizu.treep.parser.Parser
import com.github.kmizu.treep.east.Normalize
import com.github.kmizu.treep.east.Element

class MacroSpec extends FunSuite {

  private def find(e: Element, kind: String): List[Element] = {
    val here = if (e.kind == kind) List(e) else Nil
    here ++ e.children.flatMap(ch => find(ch, kind))
  }

  test("for-in expands to block(let it; while(hasNext(it)) { let x = next(it); ... })") {
    val src = "def f() returns: Int { for (x in: [1,2]) { return x } }"
    val east0 = Normalize.toEAST(Parser.parseProgram(src))
    val east = MacroExpander.expand(east0)
    val blocks = find(east, "block")
    assert(blocks.nonEmpty)
    val lets = find(east, "let")
    assert(lets.exists(_.name.exists(_.startsWith("__it$"))))
    val whiles = find(east, "while")
    assert(whiles.nonEmpty)
    val conds = whiles.head.children.head
    assertEquals(conds.kind, "cond")
  }

  test("nested for-in gets distinct iterator gensyms") {
    val src = "def g() returns: Int { for (x in: [1,2]) { for (y in: [3]) { return x } } }"
    val east = Macro.expand(Normalize.toEAST(Parser.parseProgram(src)))
    val lets = find(east, "let").flatMap(_.name.toList).filter(_.startsWith("__it$"))
    assertEquals(lets.distinct.size, lets.size)
    assert(lets.size >= 2)
  }
}

