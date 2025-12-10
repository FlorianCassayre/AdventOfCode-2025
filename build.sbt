name := "AdventOfCode-2025"

version := "0.1"

scalaVersion := "3.7.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

libraryDependencies ++= Seq(
  "com.github.vagmcs" %% "optimus" % "3.4.5" exclude("ch.qos.logback", "logback-classic"),
  "com.github.vagmcs" %% "optimus-solver-oj" % "3.4.5" exclude("ch.qos.logback", "logback-classic"),
  "org.slf4j" % "slf4j-nop" % "2.0.17"
)
