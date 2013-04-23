import sbt._
import Keys._

object MatchlessBuild extends Build {

  lazy val main = Project(
    id        = "main",
    base      = file( "." )
  )

  // Sometimes libraries, like specs2, have a latest version for older Scala versions
  val scala290 = """2\.9\.0.*""".r
  val scala29 = """2\.9.*""".r

  def specs2Dependencies(scalaVersion: String, context: String = "test") = {
    scalaVersion match {
      case scala290() => Seq(
        "org.specs2" %% "specs2" % "1.7.1" % context
      )
      case scala29() => Seq(
        "org.specs2" %% "specs2" % "1.12.3" % context
      )
      case _ => Seq(
        "org.specs2" % "specs2_2.10" % "1.14" % context // current version for Scala 2.10.x
      )
    }
  }

}

