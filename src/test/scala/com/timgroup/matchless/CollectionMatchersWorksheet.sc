package com.timgroup.matchless

import org.specs2.matcher.MustMatchers._
import com.timgroup.matchless.CollectionMatchers._

object CollectionMatchersWorksheet {
  val testMap = Map(
    "a" -> "Apple",
    "b" -> "Banana",
    "c" -> "Carrot"
  )
  
  (testMap must haveThePairs(
    "a" -> "Apple",
    "b" -> "Banana",
    "c" -> "Cactus",
    "d" -> "Dragonfruit"
  )).message
                                                  
  (testMap must havePairsLike(
    "a" -> contain("pp"),
    "b" -> contain("anana"),
    "c" -> contain("actus")
  )).message
}