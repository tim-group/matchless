package com.timgroup.matchless

import org.specs2.matcher.MustMatchers._
import com.timgroup.matchless.CollectionMatchers._

object CollectionMatchersWorksheet {
  val testMap = Map(
    "a" -> "Apple",
    "b" -> "Banana",
    "c" -> "Carrot"
  )                                               //> testMap  : scala.collection.immutable.Map[java.lang.String,java.lang.String]
                                                  //|  = Map(a -> Apple, b -> Banana, c -> Carrot)
  
  (testMap must haveThePairs(
    "a" -> "Apple",
    "b" -> "Banana",
    "c" -> "Cactus",
    "d" -> "Dragonfruit"
  )).message                                      //> res0: String = some of the expected key/value pairs were not present in the 
                                                  //| collection: 
                                                  //| 
                                                  //| a: Apple
                                                  //| b: Banana
                                                  //| * c: 'Carrot' is not equal to 'Cactus'
                                                  //| * d: <missing value>

  (testMap must havePairsLike(
    "a" -> contain("pp"),
    "b" -> contain("anana"),
    "c" -> contain("actus")
  )).message                                      //> res1: String = some of the expected key/value pairs were not present in the 
                                                  //| collection: 
                                                  //| 
                                                  //| a: Apple
                                                  //| b: Banana
                                                  //| * c: 'Carrot' doesn't contain 'actus'
  
  (List(1, 2, 3) must haveItemsLike(greaterThan(2), greaterThan(2), lessThan(2))).message
                                                  //> Map(org.specs2.matcher.Matcher$$anon$3@5954864a -> List(3), org.specs2.match
                                                  //| er.Matcher$$anon$3@3c3c9217 -> List(3), org.specs2.matcher.BeLessThan@28e70e
                                                  //| 30 -> List(1))
                                                  //| List(3)
                                                  //| Bag(Map())
                                                  //| List(Bag(Map(3 -> 1)), Bag(Map(1 -> 1)))
                                                  //| 
                                                  //| List()
                                                  //| Bag(Map(3 -> 1))
                                                  //| List(Bag(Map(1 -> 1)))
                                                  //| 
                                                  //| List()
                                                  //| Bag(Map())
                                                  //| List(Bag(Map(3 -> 1)), Bag(Map(1 -> 1)))
                                                  //| 
                                                  //| res2: String = No set of items in the collection uniquely matched the suppli
                                                  //| ed matchers
                                                  
  (List(1, 2, 3, 4) must haveItemsLike(greaterThan(2), greaterThan(1), greaterThan(1))).message
                                                  //> Map(org.specs2.matcher.Matcher$$anon$3@7d95d4fe -> List(3, 4), org.specs2.ma
                                                  //| tcher.Matcher$$anon$3@68814013 -> List(2, 3, 4), org.specs2.matcher.Matcher$
                                                  //| $anon$3@31884174 -> List(2, 3, 4))
                                                  //| List(3, 4)
                                                  //| Bag(Map())
                                                  //| List(Bag(Map(3 -> 1, 4 -> 1, 2 -> 1)), Bag(Map(3 -> 1, 4 -> 1, 2 -> 1)))
                                                  //| 
                                                  //| List(4, 2)
                                                  //| Bag(Map(3 -> 1))
                                                  //| List(Bag(Map(4 -> 1, 2 -> 1)))
                                                  //| 
                                                  //| List(2)
                                                  //| Bag(Map(3 -> 1, 4 -> 1))
                                                  //| List()
                                                  //| 
                                                  //| res3: String = The items List(3, 4, 2) uniquely matched the supplied matcher
                                                  //| s
                                                  
  (List(1, 1, 1) must haveItemsLike(lessThan(2), lessThan(3), lessThan(2))).message
                                                  //> Map(org.specs2.matcher.BeLessThan@236acdd1 -> List(1, 1, 1), org.specs2.matc
                                                  //| her.BeLessThan@36baa466 -> List(1, 1, 1), org.specs2.matcher.BeLessThan@177f
                                                  //| 409c -> List(1, 1, 1))
                                                  //| List(1, 1, 1)
                                                  //| Bag(Map())
                                                  //| List(Bag(Map(1 -> 3)), Bag(Map(1 -> 3)))
                                                  //| 
                                                  //| List(1, 1)
                                                  //| Bag(Map(1 -> 1))
                                                  //| List(Bag(Map(1 -> 2)))
                                                  //| 
                                                  //| List(1)
                                                  //| Bag(Map(1 -> 2))
                                                  //| List()
                                                  //| 
                                                  //| res4: String = The items List(1, 1, 1) uniquely matched the supplied matcher
                                                  //| s
}