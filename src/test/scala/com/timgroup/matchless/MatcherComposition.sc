package com.timgroup.matchless

import org.specs2.matcher.MustMatchers._
import org.specs2.matcher.{MatchResult, Matcher, Expectable}
import scala.collection.GenTraversableOnce

object MatcherComposition {
  
  def haveOnlyOneElementLike[T, U](f: PartialFunction[T, MatchResult[U]]) = HaveOnlyOneElementLike(f)
                                                  //> haveOnlyOneElementLike: [T, U](f: PartialFunction[T,org.specs2.matcher.Match
                                                  //| Result[U]])com.timgroup.matchless.MatcherComposition.HaveOnlyOneElementLike[
                                                  //| T,U]
  
  List(1, 2, 3) must contain(2, 3)                //> res0: org.specs2.matcher.MatchResult[List[Int]] = MatchSuccess(<function0>,<
                                                  //| function0>,org.specs2.matcher.MustExpectations$$anon$2@220ca470)
                                                  
  1 must beGreaterThan(0)                         //> res1: org.specs2.matcher.MatchResult[Int] = MatchSuccess(<function0>,<functi
                                                  //| on0>,org.specs2.matcher.MustExpectations$$anon$2@7786df0f)
                                                  
  List(1, 2, 3) must haveAllElementsLike { case i => i must beGreaterThan(0) }
                                                  //> res2: org.specs2.matcher.MatchResult[List[Int]] = MatchSuccess(<function0>,<
                                                  //| function0>,org.specs2.matcher.MustExpectations$$anon$2@4979935d)
                                                  
  List(1, 2, 3) must haveOneElementLike { case i => i must beGreaterThan(0) }
                                                  //> res3: org.specs2.matcher.MatchResult[List[Int]] = MatchSuccess(<function0>,<
                                                  //| function0>,org.specs2.matcher.MustExpectations$$anon$2@313a53d)
                                                  
  (List(1, 2, 3) must haveOnlyOneElementLike { case i => i must beGreaterThan(2) }).message
                                                  //> res4: String = only 3 is greater than 2
                                                  
  (List(1, 2, 3) must haveOnlyOneElementLike { case i => i must beGreaterThan(1) }).message
                                                  //> res5: String = 2 is greater than 1 and 3 is greater than 1
                                                  
  
  
  
  case class HaveOnlyOneElementLike[T, U](f: PartialFunction[T, MatchResult[U]])
    extends Matcher[GenTraversableOnce[T]] {
    def apply[S <: GenTraversableOnce[T]](s: Expectable[S]) = {
      val successes = s.value.seq.filter(e => f(e).isSuccess).toList
      result(successes.length == 1,
             "only " + f(successes.head).message,
             successes.map(f(_).message).mkString(" and "),
             s)
    }
  }
}