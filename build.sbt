val scala3Version = "3.0.1"

name := "morphological-parser-finnish"
version := "0.1.0"
scalaVersion := scala3Version
libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.2.9" % "test",
  "com.typesafe.akka" % "akka-actor_2.13" % "2.6.15")


