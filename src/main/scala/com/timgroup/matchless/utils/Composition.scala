package com.timgroup.matchless

import org.specs2.execute.Result
import org.specs2.SpecificationWithJUnit
import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import org.specs2.matcher.Matchers

trait Composition extends Matchers {
  def allOf(results: Result*): Result = allOf(results.toSeq)
  def allOf(results: Iterable[Result]): Result = results.reduceLeft( (r1: Result, r2: Result) => r1.and(r2))
  def matchAll[A](matchers: Matcher[A]*): Matcher[A] = new Matcher[A] {
    def apply[S <: A](t: Expectable[S]): MatchResult[S] = {
      val results = matchers.map(_.apply(t))
      results.reduceLeft( (r1: MatchResult[S], r2: MatchResult[S]) => r1 and r2)
    }
  }
}

object Composition extends Composition