package com.github.kmizu.treep.`macro`

import com.github.kmizu.treep.east.*
import scala.util.matching.Regex

/** Pattern matching and substitution for user-defined macros */
object MacroPattern:

  /** Represents a parsed macro pattern */
  case class ParsedPattern(
    macroName: String,
    variables: List[PatternVar],
    structureHint: String
  )

  /** Represents a pattern variable with optional type hint */
  case class PatternVar(
    name: String,        // without the $ prefix
    typeHint: Option[String],  // e.g., "Expr", "Block", "Stmt"
    position: Int        // position in the pattern
  )

  /** Binding from pattern variable to actual EAST element */
  type Bindings = Map[String, Element]

  private val varPattern: Regex = """\$([a-zA-Z_][a-zA-Z0-9_]*)(?:\s*:\s*([a-zA-Z_][a-zA-Z0-9_]*))?""".r

  /** Parse a macro pattern string into structured form */
  def parsePattern(pattern: String): ParsedPattern =
    // Extract macro name (first identifier before '(')
    val nameMatch = """^([a-zA-Z_][a-zA-Z0-9_]*)\s*\(""".r.findFirstMatchIn(pattern)
    val macroName = nameMatch.map(_.group(1)).getOrElse("unknown")

    // Extract all pattern variables
    val variables = scala.collection.mutable.ListBuffer.empty[PatternVar]
    var pos = 0
    varPattern.findAllMatchIn(pattern).foreach { m =>
      val varName = m.group(1)
      val typeHint = Option(m.group(2))
      variables += PatternVar(varName, typeHint, pos)
      pos += 1
    }

    ParsedPattern(macroName, variables.toList, pattern)

  /**
   * Match a call site against a parsed pattern and extract bindings
   *
   * Simple implementation:
   * - For call nodes, match arguments positionally
   * - For blocks, match the body
   */
  def matchPattern(pattern: ParsedPattern, callSite: Element): Option[Bindings] =
    callSite.kind match
      case "call" =>
        // Match function-like macro: macroName(arg1, arg2, ...)
        val name = callSite.name.getOrElse("")
        if name != pattern.macroName then return None

        // Simple positional matching of arguments
        val args = callSite.children
        val bindings = scala.collection.mutable.Map.empty[String, Element]

        // Match based on pattern variables
        pattern.variables.zipWithIndex.foreach { case (pvar, idx) =>
          if idx < args.length then
            bindings(pvar.name) = args(idx)
        }

        // Check if we have the right number of arguments
        if bindings.size == pattern.variables.length then
          Some(bindings.toMap)
        else None

      case _ =>
        // For other node types, we'd need more sophisticated matching
        // For now, only support call-style macros
        None

  /**
   * Substitute pattern variables in the expansion template
   *
   * Recursively walks the template and replaces var nodes that match
   * pattern variable names with the bound elements
   */
  def substitute(template: Element, bindings: Bindings): Element =
    template.kind match
      case "var" =>
        // Check if this variable name matches a pattern variable
        template.name match
          case Some(varName) if bindings.contains(varName) =>
            // Replace with the bound element
            bindings(varName)
          case _ =>
            // Keep the original variable
            template

      case _ =>
        // Recursively substitute in children
        template.copy(children = template.children.map(substitute(_, bindings)))

  /**
   * Expand a user-defined macro
   *
   * @param pattern The parsed macro pattern
   * @param expansion The expansion template
   * @param callSite The macro invocation site
   * @return The expanded EAST element, or the original callSite if matching fails
   */
  def expandMacro(pattern: ParsedPattern, expansion: Element, callSite: Element): Element =
    matchPattern(pattern, callSite) match
      case Some(bindings) =>
        substitute(expansion, bindings)
      case None =>
        // Pattern didn't match, return original
        callSite
