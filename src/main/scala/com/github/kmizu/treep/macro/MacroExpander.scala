package com.github.kmizu.treep.`macro`

import com.github.kmizu.treep.east.*
import java.util.concurrent.atomic.AtomicLong

/** Expands both built-in and user-defined macros */
object MacroExpander:
  private val counter = new AtomicLong(0L)

  /** Generate a unique hygienic identifier */
  private def gensym(prefix: String = "__tmp"): String =
    s"${prefix}$$${counter.incrementAndGet()}"

  /** Main expansion entry point */
  def expand(tree: Element): Element =
    // First, extract and register user-defined macros
    val programWithoutMacros = MacroRegistry.extractAndRegister(tree)
    // Then expand all macros (built-in and user-defined)
    expandAll(programWithoutMacros)

  /** Recursively expand all macros in the tree */
  private def expandAll(el: Element): Element =
    el.kind match
      case "for" => expandBuiltinFor(el)
      case "call" =>
        // Check if this is a user-defined macro invocation
        val name = el.name.getOrElse("")
        MacroRegistry.lookup(name) match
          case Some(macroEntry) => expandUserMacro(macroEntry, el)
          case None => el.copy(children = el.children.map(expandAll))
      case _ => el.copy(children = el.children.map(expandAll))

  /** Expand the built-in for..in macro */
  private def expandBuiltinFor(node: Element): Element =
    val binder = node.attrs.find(_.key == "var").map(_.value).getOrElse("_")
    val iterChild = node.children.find(_.kind == "iter").flatMap(_.children.headOption)
      .map(expandAll) // expand inside iter expression
      .getOrElse(Element("list"))
    val bodyBlock = node.children.find(_.kind == "block").map(expandAll).getOrElse(Element("block"))

    val itName = gensym("__it")

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
        val rest = b.children.map(expandAll)
        Element("block", children = bindLet :: rest)
      case other => Element("block", children = bindLet :: List(expandAll(other)))

    val whileNode = Element("while", children = List(cond, bodyExpanded))

    Element("block", children = List(iterInit, whileNode))

  /** Expand a user-defined macro */
  private def expandUserMacro(macroEntry: MacroRegistry.MacroEntry, callSite: Element): Element =
    // Parse the pattern
    val parsedPattern = MacroPattern.parsePattern(macroEntry.pattern)

    // Get the expansion template (first child of macro block)
    val expansion = macroEntry.expansion

    // Match and expand
    val expanded = MacroPattern.expandMacro(parsedPattern, expansion, callSite)

    // Recursively expand the result in case it contains more macros
    expandAll(expanded)
