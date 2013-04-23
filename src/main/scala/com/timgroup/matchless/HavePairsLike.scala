package com.timgroup.matchless

import org.specs2.matcher.{Matcher, Expectable}
import org.specs2.matcher.MustMatchers._

case class HavePairsLike[K, V](pairMatchers: Map[K, Matcher[V]]) extends Matcher[Map[K, V]] {
  def apply[S <: Map[K, V]](s: Expectable[S]) = {
    val sharedKeys = pairMatchers.keySet.intersect(s.value.keySet)
    val missingKeys = pairMatchers.keySet.diff(s.value.keySet)

    val nonMatchingValues =
      pairMatchers.filterKeys(sharedKeys)
                  .filter { case (key, matcher) => (s.value(key) must matcher).isFailure }
                  .map    { case (key, matcher) => (key, (s.value(key) must matcher).message) }
                  .toMap

    val report = pairMatchers.keySet.map(key =>
      if (missingKeys.contains(key)) "* %s: <missing value>".format(key)
      else if (nonMatchingValues.contains(key)) "* %s: %s".format(key, nonMatchingValues(key))
      else "%s: %s".format(key, s.value(key))).mkString("\n")

    result(missingKeys.isEmpty && nonMatchingValues.isEmpty,
      report,
      "some of the expected key/value pairs were not present in the collection: \n\n" + report,
      s)
  }
}
