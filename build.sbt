scalaVersion := "3.7.3"
organization := "com.github.kmizu"
name := "treep"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked"
)

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "1.0.0" % Test
)
