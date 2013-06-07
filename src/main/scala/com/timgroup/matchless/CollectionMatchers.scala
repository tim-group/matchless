package com.timgroup.matchless

import org.specs2.matcher.{ Matcher, Expectable }
import org.specs2.matcher.MustMatchers._
import scala.collection.GenTraversableOnce
import com.timgroup.matchless.utils.Bag

trait CollectionMatchers {
  def haveThePairs[K, V](pairs: (K, V)*) = PairsLikeMatcher(pairs.map(p => (p._1, beEqualTo(p._2))).toMap)
  def havePairsLike[K, V](pairs: (K, Matcher[V])*) = PairsLikeMatcher(pairs.toMap)
  def haveItemsLike[A](itemMatchers: Matcher[A]*) = ItemsLikeMatcher(itemMatchers.toList)
}

object CollectionMatchers extends CollectionMatchers

case class PairsLikeMatcher[K, V](pairMatchers: Map[K, Matcher[V]]) extends Matcher[Map[K, V]] {
  def apply[S <: Map[K, V]](s: Expectable[S]) = {
    val sharedKeys = pairMatchers.keySet.intersect(s.value.keySet)
    val missingKeys = pairMatchers.keySet.diff(s.value.keySet)

    val nonMatchingValues =
      pairMatchers.filterKeys(sharedKeys)
        .filter { case (key, matcher) => (s.value(key) must matcher).isFailure }
        .map { case (key, matcher) => (key, (s.value(key) must matcher).message) }
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

trait Orderable[A] { self: Matcher[GenTraversableOnce[A]] =>
  def inOrder: Matcher[GenTraversableOnce[A]]
}

trait Lenient[A] { self: Matcher[GenTraversableOnce[A]] =>
  val matchers: Iterable[Matcher[A]]

  def strictly = new Matcher[GenTraversableOnce[A]] with Orderable[A] {
    override def apply[S <: GenTraversableOnce[A]](s: Expectable[S]) = makeStrict(matchers, self).apply(s)
    override def inOrder = makeStrict(matchers, ItemsLikeInOrderMatcher(matchers))
    
    def makeStrict(matchers: Iterable[Matcher[A]], innerMatcher: Matcher[GenTraversableOnce[A]]) =
      new Matcher[GenTraversableOnce[A]] {
        override def apply[S <: GenTraversableOnce[A]](s: Expectable[S]) = {
          if (matchers.size !== s.value.size)
            result(false, "", "Expected %s items, but found %s".format(matchers.size, s.value.size), s)
          else innerMatcher(s)
        }
      }
  }
}

case class ItemsLikeMatcher[A](matchers: Iterable[Matcher[A]]) extends Matcher[GenTraversableOnce[A]]
  with Lenient[A] with Orderable[A] {
  override def apply[S <: GenTraversableOnce[A]](s: Expectable[S]) = {
    if (s.value.size < matchers.size) result(false, "", "Not enough items to find a unique match for every matcher", s)
    else {
      val matches = (for {
        value <- s.value.toList
        matcher <- matchers if (value must matcher).isSuccess
      } yield matcher -> value).groupBy(_._1).mapValues(_.map(_._2))
      if (matches.size < matchers.size) result(false, "", "Some of the expected items were not present in the collection", s)
      else {
        val search = matches.values.toList.sortBy(_.size).map(Bag(_))
        val uniqueMatches = findUnique(search.head, Bag.empty, search.tail)
        uniqueMatches match {
          case None => result(false, "", "No set of items in the collection uniquely matched the supplied matchers", s)
          case Some(matches) => result(true, "The items %s uniquely matched the supplied matchers".format(matches), "", s)
        }
      }
    }
  }

  private[this] def findUnique(current: Bag[A], claimed: Bag[A], remaining: List[Bag[A]]): Option[Bag[A]] =
    remaining match {
      case Nil => current.headOption.map(claimed + _)
      case l if l.exists(_.isEmpty) => None
      case h :: t =>
        if (current.isEmpty) None
        else {
          val x = current.head
          val xs = current - x
          findUnique(h - x, claimed + x, t.map(_ - x))
            .orElse(findUnique(xs, claimed, remaining))
        }
    }

  override def inOrder = ItemsLikeInOrderMatcher(matchers)
}

case class ItemsLikeInOrderMatcher[A](matchers: Iterable[Matcher[A]]) extends Matcher[GenTraversableOnce[A]] with Lenient[A] {
  def apply[S <: GenTraversableOnce[A]](s: Expectable[S]) = {
    def checkMatchers(matchers: Iterable[Matcher[A]], values: GenTraversableOnce[A]): Boolean =
      matchers match {
        case Nil => true
        case m :: ms => values match {
          case Nil => false
          case v :: vs => if ((v must m).isSuccess) checkMatchers(ms, vs) else checkMatchers(matchers, vs)
        }
      }
    result(checkMatchers(matchers, s.value),
      "The items %s matched the supplied matchers in order".format(s.value),
      "The expected items were not present in the collection in the expected order",
      s)
  }
}