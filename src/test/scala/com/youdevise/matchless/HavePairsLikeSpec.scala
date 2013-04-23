package com.youdevise.matchless

import Collections._
import Matchers._
import org.specs2.matcher.MustMatchers._
import org.specs2.Specification

class HavePairsLikeSpec extends Specification {
  
  def is =
  "A HavePairsLike matcher" ^
    "matches a map with the required pairs" ! {
      Map(1 -> "one", 2 -> "two", 3 -> "three") must havePairsLike(
        1 -> contain("ne"),
        2 -> beEqualTo("two"),
        3 -> contain("ee")
      )
    } ^
    "fails to match a map missing any of the required keys" ! {
      havePairsLike(
        1 -> contain("ne"),
        2 -> beEqualTo("two"),
        3 -> contain("ee")
      ) must failToMatchTheValue(Map(1 -> "one", 2 -> "two")).withMessageLike(contain("* 3: <missing value>"))
    } ^
    "fails to match a map with any non-matching values" ! {
            havePairsLike(
        1 -> contain("ne"),
        2 -> beEqualTo("two"),
        3 -> contain("ee")
      ) must failToMatchTheValue(Map(1 -> "one", 2 -> "two", 3 -> "four")).withMessageLike(contain("* 3: 'four' doesn't contain 'ee'"))
    }

}