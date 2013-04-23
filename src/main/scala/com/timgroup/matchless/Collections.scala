package com.timgroup.matchless

import org.specs2.matcher.Matcher
import org.specs2.matcher.MustMatchers._

object Collections {
  def haveThePairs[K, V](pairs: (K, V)*) = HavePairsLike(pairs.map( p => (p._1, beEqualTo(p._2))).toMap)
  def havePairsLike[K, V](pairs: (K, Matcher[V])*) = HavePairsLike(pairs.toMap)
}
