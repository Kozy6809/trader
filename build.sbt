name := "trader"

version := "0.1"

scalaVersion := "2.13.8"

scalacOptions ++= Seq(
"-encoding", "UTF-8",
"-unchecked",
"-deprecation"
)


libraryDependencies += "org.seleniumhq.selenium" % "selenium-firefox-driver" % "3.141.59"

libraryDependencies += "org.seleniumhq.selenium" % "selenium-java" % "4.0.0-alpha-2"
