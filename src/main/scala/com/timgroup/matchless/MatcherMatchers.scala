package com.timgroup.matchless

import org.specs2.matcher.{Matcher, Expectable}
import org.specs2.matcher.MustMatchers._

object MatcherMatchers {
  def matchTheValue[V](value: V) = MatchTheValue(value, true)
  def failToMatchTheValue[V](value: V) = MatchTheValue(value, false)
}

case class MatchTheValue[V](value: V, shouldSucceed: Boolean, maybeMessageMatcher: Option[Matcher[String]] = None) extends Matcher[Matcher[V]] {
  def apply[S <: Matcher[V]](s: Expectable[S]) = {
    val matchResult =  value must s.value
    
    val message = matchResult.message
    val onSuccess = if (shouldSucceed) "matched" else "failed to match,"
    val onFailure = if (shouldSucceed) "failed to match" else "matched"
    val statusMatched = matchResult.isSuccess == shouldSucceed
    
    maybeMessageMatcher.map { messageMatcher =>
      val messageMatched = (message must messageMatcher).isSuccess
      val messageMatchMessage = (message must messageMatcher).message
      
      result(statusMatched && messageMatched,
             "%s with the message '%s'".format(onSuccess, matchResult.message),
             if (statusMatched) "%s but %s".format(onSuccess, messageMatchMessage)
             else ("%s with the message %s").format(onFailure, matchResult.message),
             s)
    }.getOrElse(
      result(statusMatched,
             "%s with the message '%s'".format(onSuccess, matchResult.message),
             "%s with the message '%s'".format(onFailure, matchResult.message),
             s)
    )
  }
  
  def withTheMessage(message: String) = withMessageLike(beEqualTo(message))
  def withMessageLike(messageMatcher: Matcher[String]) = this.copy(maybeMessageMatcher = Some(messageMatcher))
}
