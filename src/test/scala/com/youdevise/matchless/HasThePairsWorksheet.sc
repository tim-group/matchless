package com.youdevise.matchless

import org.specs2.matcher.MustMatchers._
import com.youdevise.matchless.Collections._

object HasThePairsWorksheet {
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
}