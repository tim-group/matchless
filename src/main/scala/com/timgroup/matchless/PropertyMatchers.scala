package com.timgroup.matchless

import org.specs2.matcher.{ Matcher, Expectable }
import org.specs2.matcher.MustMatchers._

trait PropertyMatchers {
  type NamedProperty[A, B] = (String, (A) => B)
  
  def propertyOf[A, B](name: String, accessor: (A) => B): NamedProperty[A, B] = (name, accessor)
  def nameOf[A, B](property: NamedProperty[A, B]) = property._1
  def accessorFor[A, B](property: NamedProperty[A, B]) = property._2
  
  trait PropertyMatcherBuilder[A, B] { def like(matcher: Matcher[B]): Matcher[A] }
  
  def haveProperty[A, B](name: String, accessor: (A) => B) = new PropertyMatcherBuilder[A, B] {
    def like(matcher: Matcher[B]): Matcher[A] = PropertyMatcher(name, accessor, matcher)
  }
  def haveProperty[A, B](property: NamedProperty[A, B]): PropertyMatcherBuilder[A, B] = haveProperty(nameOf(property), accessorFor(property))

  def haveProperty[A, B](name: String, accessor: (A) => B, literal: B): Matcher[A] = PropertyMatcher(name, accessor, beEqualTo(literal))
  def haveProperty[A, B](property: NamedProperty[A, B], literal: B): Matcher[A] = haveProperty(nameOf(property), accessorFor(property), literal)

  type NamedPropertyWithLiteral[A, B] = (NamedProperty[A, B], B)
  def toNamedPropertyWithMatcher[A, B](namedPropertyWithLiteral: NamedPropertyWithLiteral[A, B]) =
    (namedPropertyWithLiteral._1, beEqualTo(namedPropertyWithLiteral._2))
    
  type NamedPropertyWithMatcher[A, B] = (NamedProperty[A, B], Matcher[B])
  def toPropertyMatcher[A, B](namedPropertyWithMatcher: NamedPropertyWithMatcher[A, B]): PropertyMatcher[A, B] =
    PropertyMatcher(nameOf(namedPropertyWithMatcher._1), accessorFor(namedPropertyWithMatcher._1), namedPropertyWithMatcher._2)
    
  def haveProperties[A](firstProperty: NamedPropertyWithLiteral[A, _], remainingProperties: NamedPropertyWithLiteral[A, _]*): Matcher[A] =
    havePropertiesLike(toNamedPropertyWithMatcher(firstProperty), remainingProperties.map(toNamedPropertyWithMatcher(_)):_*)
  
  def havePropertiesLike[A](firstProperty: NamedPropertyWithMatcher[A, _], remainingProperties: NamedPropertyWithMatcher[A, _]*): Matcher[A] =
    PropertiesMatcher((firstProperty :: remainingProperties.toList).map(toPropertyMatcher(_)):_*)

}

object PropertyMatchers extends PropertyMatchers

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