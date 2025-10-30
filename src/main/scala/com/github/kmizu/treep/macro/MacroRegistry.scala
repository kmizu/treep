package com.github.kmizu.treep.`macro`

import com.github.kmizu.treep.east.*
import scala.collection.mutable

/** Registry for user-defined macros */
object MacroRegistry:

  /** Represents a macro definition */
  case class MacroEntry(
    name: String,
    pattern: String,        // Pattern string (e.g., "unless($cond: Expr) { $body: Block }")
    expansion: Element      // Expansion template (Block element)
  )

  private val macros = mutable.Map[String, MacroEntry]()

  /** Register a macro */
  def register(name: String, pattern: String, expansion: Element): Unit =
    macros(name) = MacroEntry(name, pattern, expansion)

  /** Look up a macro by name */
  def lookup(name: String): Option[MacroEntry] =
    macros.get(name)

  /** Check if a macro is registered */
  def isDefined(name: String): Boolean =
    macros.contains(name)

  /** Get all registered macro names */
  def allMacros: List[String] =
    macros.keys.toList

  /** Clear all registered macros (useful for testing) */
  def clear(): Unit =
    macros.clear()

  /** Extract macro definitions from a program and register them */
  def extractAndRegister(program: Element): Element =
    val nonMacroChildren = scala.collection.mutable.ListBuffer.empty[Element]

    program.children.foreach { child =>
      if child.kind == "macro" then
        // Register this macro
        val name = child.name.getOrElse("?")
        val pattern = child.getAttr("pattern").getOrElse("")
        val expansion = child.children.headOption.getOrElse(Element("block"))
        register(name, pattern, expansion)
      else
        // Keep non-macro declarations
        nonMacroChildren += child
    }

    // Return program without macro definitions
    program.copy(children = nonMacroChildren.toList)
