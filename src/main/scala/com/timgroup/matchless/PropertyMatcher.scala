package com.timgroup.matchless

import org.specs2.matcher.{ Matcher, Expectable }
import org.specs2.matcher.MustMatchers._

case class PropertyMatcher[A, B](name: String, accessor: (A) => B, matcher: Matcher[B]) extends Matcher[A] {
  def apply[S <: A](s: Expectable[S]) = {
    val propertyValue = accessor.apply(s.value)
    val matchResult = propertyValue must matcher
    val success = matchResult.isSuccess
    val report = "The property <%s> of %s %s".format(
      name,
      s.description,
      if (success) "was %s".format(propertyValue)
      else "doesn't match the expectation: %s".format(matchResult.message))
    result(matchResult.isSuccess,
      report,
      report,
      s)
  }
}