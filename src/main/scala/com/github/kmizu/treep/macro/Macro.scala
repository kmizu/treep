package com.github.kmizu.treep.`macro`

import com.github.kmizu.treep.east.*
import java.util.concurrent.atomic.AtomicLong

object Macro:
  private val counter = new AtomicLong(0L)
  private def gensym(prefix: String = "__it"): String =
    s"${prefix}$$${counter.incrementAndGet()}"

  def expand(tree: Element): Element =
    def loop(el: Element): Element = el.kind match
      case "for" => expandFor(el)
      case _ => el.copy(children = el.children.map(loop))
    loop(tree)

  private def expandFor(node: Element): Element =
    val binder = node.attrs.find(_.key == "var").map(_.value).getOrElse("_")
    val iterChild = node.children.find(_.kind == "iter").flatMap(_.children.headOption)
      .map(expand) // expand inside iter expression
      .getOrElse(Element("list"))
    val bodyBlock = node.children.find(_.kind == "block").map(expand).getOrElse(Element("block"))

    val itName = gensym()

    val iterInit = Element(
      kind = "let",
      name = Some(itName),
      children = List(Element("init", children = List(Element("call", name = Some("iter"), children = List(iterChild)))))
    )

    val cond = Element("cond", children = List(Element("call", name = Some("hasNext"), children = List(Element("var", name = Some(itName))))))

    val bindLet = Element(
      kind = "let",
      name = Some(binder),
      children = List(Element("init", children = List(Element("call", name = Some("next"), children = List(Element("var", name = Some(itName)))))))
    )

    val bodyExpanded = bodyBlock match
      case b if b.kind == "block" =>
        val rest = b.children.map(expand)
        Element("block", children = bindLet :: rest)
      case other => Element("block", children = bindLet :: List(expand(other)))

    val whileNode = Element("while", children = List(cond, bodyExpanded))

    Element("block", children = List(iterInit, whileNode))

