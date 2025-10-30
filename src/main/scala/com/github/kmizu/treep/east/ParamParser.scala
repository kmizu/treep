package com.github.kmizu.treep.east

object ParamParser:
  /**
   * Parse parameter list string like "x: Int, y: String" into list of (name, type) pairs
   * Returns None if parsing fails for a parameter
   */
  def parseParams(paramsStr: String): List[(String, String)] =
    if paramsStr.trim.isEmpty then Nil
    else
      paramsStr.split(",").toList.flatMap { param =>
        val parts = param.trim.split(":").map(_.trim)
        if parts.length == 2 then Some((parts(0), parts(1)))
        else None
      }

  /**
   * Extract just the parameter names from a parameter list string
   */
  def parseParamNames(paramsStr: String): List[String] =
    if paramsStr.trim.isEmpty then Nil
    else
      paramsStr.split(",").toList.map { param =>
        val colonIdx = param.indexOf(":")
        if colonIdx > 0 then param.substring(0, colonIdx).trim
        else param.trim
      }.filter(_.nonEmpty)

  /**
   * Extract just the parameter types from a parameter list string
   */
  def parseParamTypes(paramsStr: String): List[String] =
    if paramsStr.trim.isEmpty then Nil
    else
      paramsStr.split(",").toList.flatMap { param =>
        val colonIdx = param.indexOf(":")
        if colonIdx > 0 && colonIdx + 1 < param.length then
          Some(param.substring(colonIdx + 1).trim)
        else None
      }.filter(_.nonEmpty)
