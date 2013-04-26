package com.timgroup.matchless

import org.specs2.matcher.{ Matcher, Expectable }
import org.specs2.matcher.MustMatchers._

case class PropertiesMatcher[A](properties: PropertyMatcher[A, _]*) extends Matcher[A] {
  def apply[S <: A](s: Expectable[S]) = {
    val success = properties.forall(_.apply(s).isSuccess)
    val report = "The properties of %s %s:\n%s".format(
      s.description,
      if (success) "were"
      else "didn't match all expectations",
      properties.map { property =>
        val propertyValue = property.accessor.apply(s.value)
        val matchResult = propertyValue must property.matcher.asInstanceOf[Matcher[Any]]
        if (matchResult.isSuccess) "<%s>: %s".format(property.name, propertyValue)
        else "* <%s>: %s".format(property.name, matchResult.message)
      }.mkString("\n"))

    result(success,
      report,
      report,
      s)
  }
}