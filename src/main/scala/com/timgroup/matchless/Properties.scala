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

  case class NamedProperty[A, B](name: String, accessor: (A) => B)
  def propertyOf[A, B](name: String, accessor: (A) => B) = NamedProperty(name, accessor)
  
  trait PropertyMatcherBuilder[A, B] { def like(matcher: Matcher[B]): Matcher[A] }
  def haveProperty[A, B](name: String, accessor: (A) => B) = new PropertyMatcherBuilder[A, B] {
    def like(matcher: Matcher[B]): Matcher[A] = PropertyMatcher(name, accessor, matcher)
  }
  def haveProperty[A, B](property: NamedProperty[A, B]): PropertyMatcherBuilder[A, B] = haveProperty(property.name, property.accessor)

  def haveProperty[A, B](name: String, accessor: (A) => B, literal: B): Matcher[A] = PropertyMatcher(name, accessor, beEqualTo(literal))
  def haveProperty[A, B](property: NamedProperty[A, B], literal: B): Matcher[A] = PropertyMatcher(property.name, property.accessor, beEqualTo(literal))

  type NameAccessorLiteral[A, B] = (String, (A) => B, B)
  def toNameAccessorMatcher[A, B](nal: NameAccessorLiteral[A, B]) = (nal._1, nal._2, beEqualTo(nal._3))
  
  def haveProperties[A](firstProperty: NameAccessorLiteral[A, _], remainingProperties: NameAccessorLiteral[A, _]*): Matcher[A] = havePropertiesLike(
      toNameAccessorMatcher(firstProperty), remainingProperties.map(toNameAccessorMatcher(_)):_*)
  
  type NamedPropertyLiteral[A, B] = (NamedProperty[A, B], B)
  implicit def namedPropertyLiteral2NameAccessorLiteral[A, B](npl: NamedPropertyLiteral[A, B]): NameAccessorLiteral[A, B] = (npl._1.name, npl._1.accessor, npl._2)

  type NameAccessorMatcher[A, B] = (String, (A) => B, Matcher[B])
  def havePropertiesLike[A](firstProperty: NameAccessorMatcher[A, _], remainingProperties: NameAccessorMatcher[A, _]*): Matcher[A] = havePropertiesMatching(
      (firstProperty :: remainingProperties.toList).map {
    case (name, accessor, matcher) => PropertyMatcher(name, accessor, matcher.asInstanceOf[Matcher[Any]])
  }: _*)
  
  type NamedPropertyMatcher[A, B] = (NamedProperty[A, B], Matcher[B])
  implicit def namedPropertyMatcher2NameAccessorMatcher[A, B](npm: NamedPropertyMatcher[A, B]): NameAccessorMatcher[A, B] = (npm._1.name, npm._1.accessor, npm._2)

  def havePropertiesMatching[A](properties: PropertyMatcher[A, _]*): Matcher[A] = new Matcher[A] {
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

}