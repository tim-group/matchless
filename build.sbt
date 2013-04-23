name := "matchless"

organization := "com.youdevise"

// Change to non-SNAPSHOT version to publish a release
//version := "1.0.0"
version := "1.0.0-SNAPSHOT"

scalaVersion := "2.9.1"

scalacOptions ++= Seq("-unchecked", "-deprecation")

crossScalaVersions := Seq("2.9.0-1", "2.9.1", "2.9.2", "2.10.0")

libraryDependencies <++= scalaVersion(specs2Dependencies(_, "compile"))
