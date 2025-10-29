package com.github.kmizu.treep.types

import munit.FunSuite
import com.github.kmizu.treep.east.*

class HMSpec extends FunSuite {
  test("infer x + y with annotated env") {
    val env0 = HM.builtinEnv
    val env = env0
      .extend("x", Scheme(Set.empty, Type.TInt))
      .extend("y", Scheme(Set.empty, Type.TInt))
    val expr = Element("call", name = Some("+"), children = List(Element("var", name = Some("x")), Element("var", name = Some("y"))))
    val res = HM.inferExpr(env, expr)
    assert(res.isRight)
    assertEquals(res.toOption.get.ty, Type.TInt)
  }

  test("iter on non-iterable is error") {
    val env = HM.builtinEnv
    val expr = Element("call", name = Some("iter"), children = List(Element("int", attrs = List(Attr("value","1")))))
    val res = HM.inferExpr(env, expr)
    assert(res.isLeft)
  }

  test("index typing for list and dict") {
    val env = HM.builtinEnv
    val listIdx = Element("index", children = List(
      Element("target", children = List(Element("list", children = List(Element("int", attrs = List(Attr("value","1"))))))),
      Element("key", children = List(Element("int", attrs = List(Attr("value","0")))))
    ))
    val r1 = HM.inferExpr(env, listIdx)
    assert(r1.isRight)
    val dictIdx = Element("index", children = List(
      Element("target", children = List(Element("dict", children = List(Element("pair", attrs = List(Attr("key","a")), children = List(Element("int", attrs = List(Attr("value","1"))))))))),
      Element("key", children = List(Element("string", attrs = List(Attr("value","a")))))
    ))
    val r2 = HM.inferExpr(env, dictIdx)
    assert(r2.isRight)
  }
}
