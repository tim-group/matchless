name := "matchless"

version := "1.0"

scalaVersion := "2.9.2"

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies ++=  Seq(
  "org.specs2" %% "specs2" % "1.12.3" % "compile"
)
