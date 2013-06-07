package com.timgroup.matchless

import CollectionMatchers._
import MatcherMatchers._
import org.specs2.matcher.MustMatchers._
import org.specs2.Specification

class CollectionMatchersSpec extends Specification {
  
  def is =
  "A PairsLikeMatcher" ^
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
    } ^ end ^
   bt ^ 
  "An ItemsLikeMatcher" ^
    "matches items in any order" ! {
      Set(1, 2, 3) must haveItemsLike(greaterThan(2), greaterThan(1))
    } ^
    "must find one distinct item matching each matcher in order to match" ! {
      haveItemsLike(greaterThan(1), greaterThan(1)) must failToMatchTheValue(List(1, 2))
    } ^
    "can be modified to match items in order" ! {
      (List(1, 2, 3) must haveItemsLike(greaterThan(1), greaterThan(2))) and
      (haveItemsLike(greaterThan(2), greaterThan(1)).inOrder must failToMatchTheValue(List(1, 2, 3)))
    } ^ 
    "can be made strict" ! {
      (List(1, 2, 3) must haveItemsLike(equalTo(1), equalTo(2))) and
      (haveItemsLike(equalTo(1), equalTo(2)).strictly must failToMatchTheValue(List(1, 2, 3)))
    } ^
    "can be made strict and ordered" ! {
      (List(1, 2, 3) must haveItemsLike(equalTo(3), equalTo(2), equalTo(1)).strictly) and
      (haveItemsLike(equalTo(3), equalTo(2), equalTo(1)).strictly.inOrder must failToMatchTheValue(List(1, 2, 3)))
    }

}
