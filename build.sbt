ThisBuild / scalaVersion := "3.5.0"
ThisBuild / organization := "com.github.kmizu"
ThisBuild / name := "treep"

lazy val root = (project in file(".")).aggregate(lexer, parser, east, `macro`, types, interpreter, cli, tests)

lazy val commonSettings = Seq(
  Compile / scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked"
  )
)

lazy val lexer  = (project in file("modules/lexer")).settings(commonSettings)
lazy val parser = (project in file("modules/parser")).dependsOn(lexer).settings(commonSettings)
lazy val east   = (project in file("modules/east")).dependsOn(parser).settings(commonSettings)
lazy val `macro`= (project in file("modules/macro")).dependsOn(east).settings(commonSettings)
lazy val types  = (project in file("modules/types")).dependsOn(`macro`).settings(commonSettings)
lazy val interpreter = (project in file("modules/interpreter")).dependsOn(types).settings(commonSettings)
lazy val cli    = (project in file("modules/cli")).dependsOn(interpreter).settings(commonSettings)
lazy val tests  = (project in file("modules/tests")).dependsOn(cli).settings(commonSettings).settings(
  libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
)
