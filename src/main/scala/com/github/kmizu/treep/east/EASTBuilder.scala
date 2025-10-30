package com.github.kmizu.treep.east

/** Type-safe EAST construction utilities for macros */
object EASTBuilder:

  // Expression builders

  def makeVar(name: String): Element =
    Element(kind = "var", name = Some(name))

  def makeInt(value: Int): Element =
    Element(kind = "int", attrs = List(Attr("value", value.toString)))

  def makeBool(value: Boolean): Element =
    Element(kind = "bool", attrs = List(Attr("value", value.toString)))

  def makeString(value: String): Element =
    Element(kind = "string", attrs = List(Attr("value", value)))

  def makeCall(name: String, args: List[Element]): Element =
    Element(kind = "call", name = Some(name), children = args)

  def makeMCall(receiver: Element, name: String, args: List[Element]): Element =
    Element(kind = "mcall", name = Some(name), children = receiver :: args)

  def makeList(elements: List[Element]): Element =
    Element(kind = "list", children = elements)

  def makeDict(pairs: List[(Element, Element)]): Element =
    Element(kind = "dict", children = pairs.map { case (k, v) =>
      Element(kind = "pair", children = List(k, v))
    })

  def makeLambda(params: List[(String, String)], body: Element): Element =
    val paramsStr = if params.isEmpty then ""
                    else params.map { case (n, t) => s"$n:$t" }.mkString(",")
    Element(
      kind = "lambda",
      attrs = if params.nonEmpty then List(Attr("params", paramsStr)) else Nil,
      children = List(body)
    )

  def makeIndex(target: Element, key: Element): Element =
    Element(
      kind = "index",
      children = List(
        Element("target", children = List(target)),
        Element("key", children = List(key))
      )
    )

  def makeField(target: Element, fieldName: String): Element =
    Element(
      kind = "field",
      attrs = List(Attr("name", fieldName)),
      children = List(target)
    )

  // Statement builders

  def makeBlock(stmts: List[Element]): Element =
    Element(kind = "block", children = stmts)

  def makeLet(name: String, init: Element, typeAnnot: Option[String] = None): Element =
    Element(
      kind = "let",
      name = Some(name),
      attrs = typeAnnot.map(t => Attr("type", t)).toList,
      children = List(Element("init", children = List(init)))
    )

  def makeAssign(name: String, rhs: Element): Element =
    Element(kind = "assign", name = Some(name), children = List(rhs))

  def makeReturn(expr: Element): Element =
    Element(kind = "return", children = List(expr))

  def makeExprStmt(expr: Element): Element =
    Element(kind = "expr", children = List(expr))

  def makeIf(cond: Element, thenBranch: Element, elseBranch: Option[Element] = None): Element =
    val elseBlock = elseBranch.getOrElse(makeBlock(Nil))
    Element(
      kind = "if",
      children = List(
        Element("cond", children = List(cond)),
        thenBranch,
        elseBlock
      )
    )

  def makeWhile(cond: Element, body: Element): Element =
    Element(
      kind = "while",
      children = List(
        Element("cond", children = List(cond)),
        body
      )
    )

  def makeFor(varName: String, iterable: Element, body: Element): Element =
    Element(
      kind = "for",
      attrs = List(Attr("var", varName)),
      children = List(
        Element("iter", children = List(iterable)),
        body
      )
    )

  def makeMatch(target: Element, cases: List[(Element, Element)]): Element =
    Element(
      kind = "match",
      children = Element("target", children = List(target)) :: cases.map { case (pattern, body) =>
        Element("case", children = List(pattern, body))
      }
    )

  // Pattern builders

  def makePInt(value: Int): Element =
    Element(kind = "pint", attrs = List(Attr("value", value.toString)))

  def makePStr(value: String): Element =
    Element(kind = "pstr", attrs = List(Attr("value", value)))

  def makePBool(value: Boolean): Element =
    Element(kind = "pbool", attrs = List(Attr("value", value.toString)))

  def makePVar(name: String): Element =
    Element(kind = "pvar", name = Some(name))

  def makePWild(): Element =
    Element(kind = "pwild")

  // Top-level declaration builders

  def makeDef(
    name: String,
    params: List[(String, String)],
    returnType: Option[String],
    body: Element
  ): Element =
    val paramsStr = if params.isEmpty then ""
                    else params.map { case (n, t) => s"$n:$t" }.mkString(",")
    Element(
      kind = "def",
      name = Some(name),
      attrs = (if params.nonEmpty then List(Attr("params", paramsStr)) else Nil) ++
              returnType.map(r => Attr("returns", r)).toList,
      children = List(body)
    )

  def makeConst(name: String, init: Element, typeAnnot: Option[String] = None): Element =
    Element(
      kind = "const",
      name = Some(name),
      attrs = typeAnnot.map(t => Attr("type", t)).toList,
      children = List(Element("init", children = List(init)))
    )

  // Validation utilities

  def isWellFormed(elem: Element): Boolean =
    elem.kind match
      case "var" => elem.name.isDefined
      case "call" => elem.name.isDefined
      case "mcall" => elem.name.isDefined && elem.children.nonEmpty
      case "let" => elem.name.isDefined && elem.children.exists(_.kind == "init")
      case "assign" => elem.name.isDefined && elem.children.nonEmpty
      case "if" => elem.children.exists(_.kind == "cond") &&
                   elem.children.count(_.kind == "block") >= 1
      case "while" => elem.children.exists(_.kind == "cond") &&
                      elem.children.exists(_.kind == "block")
      case "for" => elem.attrs.exists(_.key == "var") &&
                    elem.children.exists(_.kind == "iter") &&
                    elem.children.exists(_.kind == "block")
      case "def" => elem.name.isDefined && elem.children.nonEmpty
      case "const" => elem.name.isDefined && elem.children.exists(_.kind == "init")
      case _ => true // Other kinds assumed to be well-formed
