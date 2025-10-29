package com.github.kmizu.treep.cli

import java.nio.file.*
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters.*

import com.github.kmizu.treep.parser.Parser
import com.github.kmizu.treep.`macro`.Macro
import com.github.kmizu.treep.interpreter.Interpreter
import com.github.kmizu.treep.east.Normalize
import com.github.kmizu.treep.types.Checker

object Main:
  def main(args: Array[String]): Unit =
    if args.isEmpty then
      usage(); return
    args(0) match
      case "new"  => cmdNew(args.drop(1))
      case "build"=> cmdBuild(args.drop(1))
      case "run"  => cmdRun(args.drop(1))
      case "fmt"  => cmdFmt(args.drop(1))
      case "test" => cmdTest(args.drop(1))
      case _      => usage()

  private def usage(): Unit =
    println("treep new|build|run|fmt|test")

  private def cmdNew(rest: Array[String]): Unit =
    val path = Paths.get("samples/hello.treep")
    Files.createDirectories(path.getParent)
    val content = """
def main() returns: Unit {
  return
}
""".stripLeading()
    Files.writeString(path, content, StandardCharsets.UTF_8)
    println(s"Created ${path}")

  private def cmdBuild(rest: Array[String]): Unit =
    val cwd = Paths.get("")
    val matcher = FileSystems.getDefault.getPathMatcher("glob:**/*.treep")
    val outDir = cwd.resolve("out")
    Files.createDirectories(outDir)
    val stream = Files.walk(cwd)
    try
      val files = stream.iterator().asScala.toList.filter(p => Files.isRegularFile(p) && matcher.matches(p))
      if files.isEmpty then
        println("No .treep files found. Try `treep new`.")
      else
        // Build フェーズは構文/マクロ展開の検証のみ（生成物なし）
        files.foreach { in =>
          val src = Files.readString(in)
          val cst = Parser.parseProgram(src, in.toString)
          val east = Normalize.toEAST(cst)
          val expanded = Macro.expand(east)
          val typeDiags = Checker.check(expanded)
          val errs = Parser.lastErrors
          if errs.nonEmpty then
            println(s"Checked ${in} with ${errs.size} parse error(s):")
            errs.foreach(e => println(s"  [parse] ${e.message} at ${e.line}:${e.col}"))
          if typeDiags.nonEmpty then
            println(s"Typecheck reported ${typeDiags.size} issue(s)")
          else
            println(s"Checked ${in}")
        }
        println("Build succeeded.")
    finally
      stream.close()

  private def cmdRun(rest: Array[String]): Unit =
    val cwd = Paths.get("")
    val matcher = FileSystems.getDefault.getPathMatcher("glob:**/*.treep")
    val stream = Files.walk(cwd)
    try
      val files = stream.iterator().asScala.toList.filter(p => Files.isRegularFile(p) && matcher.matches(p))
      if files.isEmpty then
        println("No .treep files found. Try `treep new`.")
      else
        files.foreach { in =>
          val src = Files.readString(in)
          val cst = Parser.parseProgram(src, in.toString)
          val east = Normalize.toEAST(cst)
          val expanded = Macro.expand(east)
          val typeDiags = Checker.check(expanded)
          val errs = Parser.lastErrors
          if errs.nonEmpty then
            println(s"Parsing had ${errs.size} error(s), attempting to run anyway...")
            errs.foreach(e => println(s"  [parse] ${e.message} at ${e.line}:${e.col}"))
          if typeDiags.nonEmpty then
            println(s"Typecheck reported ${typeDiags.size} issue(s), attempting to run anyway...")
          Interpreter.run(expanded)
        }
    finally
      stream.close()

  private def cmdFmt(rest: Array[String]): Unit =
    println("Formatter MVP is a no-op for now.")

  private def cmdTest(rest: Array[String]): Unit =
    println("Tests are not wired yet. See modules/tests and tests/golden plan.")
