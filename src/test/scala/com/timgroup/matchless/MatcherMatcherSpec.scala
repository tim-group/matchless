package com.timgroup.matchless

import MatcherMatchers._
import org.specs2.matcher.MustMatchers._
import org.specs2.Specification

class MatcherMatcherSpec extends Specification {
  
  def is =
  "A MatcherMatcher expecting success" ^
    "matches when the matcher matches the value" ! {
      contain("hello") must matchTheValue("hello world")
    } ^
    "fails to match when the matcher doesn't match the value" ! {
      matchTheValue("goodbye sweetheart") must failToMatchTheValue(contain("hello"))
    } ^ end ^
  "A MatcherMatcher expecting failure" ^
    "matches when the matcher fails to match the value" ! {
      contain("xyzzy") must failToMatchTheValue("abracadabra")
    } ^
    "fails to match when the matcher matches the value" ! {
      failToMatchTheValue("abracadabra") must failToMatchTheValue(contain("braca"))
    } ^ end ^
  "A MatcherMatcher expecting a failure message" ^
    "matches when the matcher fails, but the failure message matches" ! {
      contain("hello") must failToMatchTheValue("goodbye sweetheart").withTheMessage("'goodbye sweetheart' doesn't contain 'hello'")
    } ^
    "fails to match when the matcher fails, but the failure message does not match" ! {
      failToMatchTheValue("goodbye sweetheart").withMessageLike(contain("eels up inside yer")) must
        failToMatchTheValue(contain("hello")).withMessageLike(contain("doesn't contain 'eels up inside yer'"))
    } ^ end

}
