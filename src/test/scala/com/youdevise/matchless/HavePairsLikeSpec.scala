package com.youdevise.matchless

import Collections._
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
  }

}