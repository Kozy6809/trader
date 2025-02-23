name := "trader"

version := "0.1"

scalaVersion := "2.13.13"

scalacOptions ++= Seq(
"-encoding", "UTF-8",
"-unchecked",
"-deprecation"
)

libraryDependencies += "ch.epfl.scala" % "sbt-bloop_2.12_1.0" % "1.5.15"

libraryDependencies += "org.seleniumhq.selenium" % "selenium-firefox-driver" % "4.27.0"

libraryDependencies += "org.seleniumhq.selenium" % "selenium-java" % "4.27.0"

libraryDependencies += "com.titusfortner" % "selenium-logger" % "2.3.0"
