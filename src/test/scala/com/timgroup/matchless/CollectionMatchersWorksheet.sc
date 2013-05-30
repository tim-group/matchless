package com.timgroup.matchless

import org.specs2.matcher.MustMatchers._

object CollectionMatchersWorksheet extends CollectionMatchers {
  
  val testMap = Map(
    "a" -> "Apple",
    "b" -> "Banana",
    "c" -> "Carrot"
  )                                               //> testMap  : scala.collection.immutable.Map[String,String] = Map(a -> Apple, b
                                                  //|  -> Banana, c -> Carrot)
                            
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
                                                  //> res2: String = No set of items in the collection uniquely matched the suppli
                                                  //| ed matchers
                                                  
  (List(1, 2, 3, 4) must haveItemsLike(greaterThan(2), greaterThan(1), greaterThan(1))).message
                                                  //> res3: String = The items Bag(4, 2, 3) uniquely matched the supplied matchers
                                                  //| 
                                                  
  (List(1, 1, 1) must haveItemsLike(lessThan(2), lessThan(3), lessThan(2))).message
                                                  //> res4: String = The items Bag(1, 1, 1) uniquely matched the supplied matchers
                                                  //| 
}