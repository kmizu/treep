package com.github.kmizu.treep.east

final case class SourceSpan(file: String, line: Int, col: Int)

final case class Attr(key: String, value: String)

final case class Element(
  kind: String,
  name: Option[String] = None,
  attrs: List[Attr] = Nil,
  children: List[Element] = Nil,
  span: Option[SourceSpan] = None
)

object Element:
  def simple(kind: String, name: String): Element = Element(kind, Some(name))

