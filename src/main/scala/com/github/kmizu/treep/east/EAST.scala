package com.github.kmizu.treep.east

final case class SourceSpan(file: String, line: Int, col: Int)

final case class Attr(key: String, value: String)

final case class Element(
  kind: String,
  name: Option[String] = None,
  attrs: List[Attr] = Nil,
  children: List[Element] = Nil,
  span: Option[SourceSpan] = None
):
  /** Get the first child element of the specified kind */
  def getChild(kind: String): Option[Element] =
    children.find(_.kind == kind).flatMap(_.children.headOption)

  /** Get the value of an attribute by key */
  def getAttr(key: String): Option[String] =
    attrs.find(_.key == key).map(_.value)

  /** Get the value of an attribute by key, or return a default value */
  def getAttrOrElse(key: String, default: String): String =
    getAttr(key).getOrElse(default)

object Element:
  def simple(kind: String, name: String): Element = Element(kind, Some(name))

