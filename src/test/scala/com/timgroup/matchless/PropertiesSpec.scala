package com.timgroup.matchless

import Properties._
import Matchers._
import org.specs2.matcher.MustMatchers._
import org.specs2.Specification

class PropertiesSpec extends Specification {
  
  case class TestClass(foo: String, bar: Int)
  val testValue = TestClass("My kingdom for a horse", 42)
  
  def is =
  "A single literal property matcher" ^ 
    "matches if the property is equal to the literal value" ! {
      testValue must haveProperty("foo", _.foo, "My kingdom for a horse")
    } ^
    "does not match if the property is not equal to the literal value" ! {
      haveProperty("foo", (v: TestClass) => v.foo, "Now is the winter of our discontent") must failToMatchTheValue(testValue)
        .withMessageLike(contain("The property <foo> of 'TestClass(My kingdom for a horse,42)' doesn't match the expectation"))
    } ^ end ^
  "A single 'like' property matcher" ^
    "matches if the property matches" ! {
      testValue must haveProperty("foo", (v: TestClass) => v.foo).like(contain("kingdom"))
    } ^
    "does not match if the property does not match" ! {
      haveProperty("foo", (v: TestClass) => v.foo).like(contain("giraffe")) must failToMatchTheValue(testValue)
        .withMessageLike(contain("The property <foo> of 'TestClass(My kingdom for a horse,42)' doesn't match the expectation"))
    } ^ end
    
}