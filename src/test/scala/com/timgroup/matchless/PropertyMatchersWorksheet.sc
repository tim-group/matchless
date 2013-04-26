package com.timgroup.matchless

import org.specs2.matcher.Matcher
import org.specs2.matcher.MustMatchers._
import com.timgroup.matchless.PropertyMatchers._

object PropertyMatchersWorksheet {
  case class Foo(bar: String, baz: Int, xyzzy: Double)
  
  val foo = Foo("a string", 42, 12.3)             //> foo  : com.timgroup.matchless.PropertyMatchersWorksheet.Foo = Foo(a string,4
                                                  //| 2,12.3)

  (foo must haveProperty("bar", _.bar, "a string")).message
                                                  //> res0: String = The property <bar> of 'Foo(a string,42,12.3)' was a string
  
  (foo must haveProperty("bar", (foo:Foo) => foo.bar).like(contain("strung"))).message
                                                  //> res1: String = The property <bar> of 'Foo(a string,42,12.3)' doesn't match t
                                                  //| he expectation: 'a string' doesn't contain 'strung'
  
  (foo must haveProperties(
    ("bar", (foo:Foo) => foo.bar) -> "a string",
    ("baz", (foo:Foo) => foo.baz) -> 44
  )).message                                      //> res2: String = The properties of 'Foo(a string,42,12.3)' didn't match all ex
                                                  //| pectations:
                                                  //| <bar>: a string
                                                  //| * <baz>: '42' is not equal to '44'
                                                  
  (foo must havePropertiesLike(
    ("bar", (foo:Foo) => foo.bar) -> contain("string"),
    ("baz", (foo:Foo) => foo.baz) -> greaterThan(42)
  )).message                                      //> res3: String = The properties of 'Foo(a string,42,12.3)' didn't match all ex
                                                  //| pectations:
                                                  //| <bar>: a string
                                                  //| * <baz>: 42 is equal to 42
                                                  
  val bar = propertyOf[Foo, String]("bar", _.bar) //> bar  : (String, com.timgroup.matchless.PropertyMatchersWorksheet.Foo => Stri
                                                  //| ng) = (bar,<function1>)
  val baz = propertyOf[Foo, Int]("baz", _.baz)    //> baz  : (String, com.timgroup.matchless.PropertyMatchersWorksheet.Foo => Int)
                                                  //|  = (baz,<function1>)
  (foo must haveProperty(bar, "a string")).message//> res4: String = The property <bar> of 'Foo(a string,42,12.3)' was a string
  (foo must haveProperty(bar).like(contain("strung"))).message
                                                  //> res5: String = The property <bar> of 'Foo(a string,42,12.3)' doesn't match 
                                                  //| the expectation: 'a string' doesn't contain 'strung'
  (foo must haveProperties(
    bar -> "a string",
    baz -> 43)).message                           //> res6: String = The properties of 'Foo(a string,42,12.3)' didn't match all e
                                                  //| xpectations:
                                                  //| <bar>: a string
                                                  //| * <baz>: '42' is not equal to '43'

  (foo must havePropertiesLike(
    bar -> contain("twine"),
    baz -> greaterThan(42))).message              //> res7: String = The properties of 'Foo(a string,42,12.3)' didn't match all e
                                                  //| xpectations:
                                                  //| * <bar>: 'a string' doesn't contain 'twine'
                                                  //| * <baz>: 42 is equal to 42
}