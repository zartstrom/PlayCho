// Turn this project into a Scala.js project by importing these settings
enablePlugins(ScalaJSPlugin)

name := "Board"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"

// persistLauncher in Compile := true
persistLauncher in Compile := false

persistLauncher in Test := false

testFrameworks += new TestFramework("utest.runner.Framework")

// append -unchecked and -deprecation to the options passed to the Scala compiler
scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.8.2",
    "com.lihaoyi" %%% "utest" % "0.3.0" % "test",
    "org.scala-lang.modules" %% "scala-async" % "0.9.1"
)
