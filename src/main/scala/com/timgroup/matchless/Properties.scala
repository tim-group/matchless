package com.timgroup.matchless

import org.specs2.matcher.{ Matcher, Expectable }
import org.specs2.matcher.MustMatchers._

object Properties {

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

  trait PropertyMatcherBuilder[A, B] { def like(matcher: Matcher[B]): Matcher[A] }
  def haveProperty[A, B](name: String, accessor: (A) => B) = new PropertyMatcherBuilder[A, B] {
    def like(matcher: Matcher[B]): Matcher[A] = PropertyMatcher(name, accessor, matcher)
  }

  def haveProperty[A, B](name: String, accessor: (A) => B, literal: B): Matcher[A] = PropertyMatcher(name, accessor, beEqualTo(literal))

  def haveProperties[A](properties: (String, (A) => _, _)*): Matcher[A] = havePropertiesLike(properties.map {
    case (name, accessor, literal) => (name, accessor, beEqualTo(literal))
  }: _*)

  def havePropertiesLike[A](properties: (String, (A) => _, Matcher[_])*): Matcher[A] = havePropertiesMatching(properties.map {
    case (name, accessor, matcher) => PropertyMatcher(name, accessor, matcher.asInstanceOf[Matcher[Any]])
  }: _*)

  def havePropertiesMatching[A](properties: PropertyMatcher[A, _]*): Matcher[A] = new Matcher[A] {
    def apply[S <: A](s: Expectable[S]) = {
      val success = properties.forall(_.apply(s).isSuccess)
      val report = "The properties of %s %s:\n\n%s".format(
        s.description,
        if (success) "were"
        else "doesn't match all expectations",
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

}