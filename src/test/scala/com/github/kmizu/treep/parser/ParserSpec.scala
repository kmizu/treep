package com.github.kmizu.treep.parser

import munit.FunSuite
import com.github.kmizu.treep.parser.CST as C

class ParserSpec extends FunSuite {

  test("parse simple function with return") {
    val src =
      """
      |def main() returns: Unit {
      |  return 0
      |}
      |""".stripMargin
    val prog = Parser.parseProgram(src)
    assertEquals(prog.tops.length, 1)
    val C.FunDef(name, params, ret, body, _) = prog.tops.head
    assertEquals(name, "main")
    assertEquals(params.isEmpty, true)
    assert(ret.exists(_.name == "Unit"))
    assert(body.stmts.nonEmpty)
  }

  test("operator precedence: 1 + 2 * 3") {
    val src = "const x: Int = 1 + 2 * 3"
    val prog = Parser.parseProgram(src)
    val C.ConstDecl(_, _, init, _) = prog.tops.head
    val bin = init.asInstanceOf[C.Binary]
    assertEquals(bin.op, "+")
    assertEquals(bin.left.asInstanceOf[C.IntLit].value, 1)
    val right = bin.right.asInstanceOf[C.Binary]
    assertEquals(right.op, "*")
    assertEquals(right.left.asInstanceOf[C.IntLit].value, 2)
    assertEquals(right.right.asInstanceOf[C.IntLit].value, 3)
  }

  test("list and dict literals") {
    val src = "const xs = [1,2]\nconst m = { \"a\": 1, b: 2 }"
    val prog = Parser.parseProgram(src)
    assertEquals(prog.tops.length, 2)
    val C.ConstDecl(_, _, listInit, _) = prog.tops.head
    val C.ListLit(es) = listInit
    assertEquals(es.length, 2)
    val C.ConstDecl(_, _, dictInit, _) = prog.tops(1)
    val C.DictLit(ps) = dictInit
    assertEquals(ps.length, 2)
    assert(ps.exists { case (k, _) => k.isInstanceOf[C.StrLit] && k.asInstanceOf[C.StrLit].value == "a" })
  }

  test("method-call node: xs.push(1) -> MethodCall(recv=xs, name=push)") {
    val src = "const r = xs.push(1)"
    val prog = Parser.parseProgram(src)
    val C.ConstDecl(_, _, init, _) = prog.tops.head
    val C.MethodCall(recv, name, args) = init
    assertEquals(name, "push")
    assertEquals(args.length, 1)
  }

  test("multi-arg lambda parses with param list") {
    val src = "const f = (x: Int, y: Int) -> { return x + y }"
    val prog = Parser.parseProgram(src)
    val C.ConstDecl(_, _, init, _) = prog.tops.head
    val lam = init.asInstanceOf[C.Lambda]
    assertEquals(lam.params.length, 2)
    assertEquals(lam.params.head.name, "x")
  }

  test("if/else and for-in in block") {
    val src =
      """
      |def f(xs: List[Int]) returns: Int {
      |  let acc: Int = 0
      |  for (i in xs) { let acc: Int = acc + i }
      |  if (acc > 0) { return acc } else { return 0 }
      |}
      |""".stripMargin
    val prog = Parser.parseProgram(src)
    val C.FunDef(_, _, _, body, _) = prog.tops.head
    assert(body.stmts.exists(_.isInstanceOf[C.ForInStmt]))
    assert(body.stmts.exists(_.isInstanceOf[C.IfStmt]))
  }

  test("error recovery between top-level decls") {
    val src = "const a = 1\n$#$#$\nconst b = 2"
    val (prog, diags) = Parser.parseProgramWithDiagnostics(src)
    assertEquals(prog.tops.length >= 2, true)
    assert(diags.nonEmpty)
  }
}
