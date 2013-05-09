package com.timgroup.matchless.utils

import scala.collection.SetLike
import scala.collection.GenTraversableLike
import scala.collection.immutable.LinearSeq
import scala.collection.SeqLike
import scala.collection.mutable.Builder

case class Bag[A](contents: Map[A, Int]) extends Iterable[A] {

  def empty = Bag.empty
  override def isEmpty = contents.isEmpty
  override def size = contents.values.sum

  def -(element: A) =
    contents.get(element) match {
      case None => this
      case Some(1) => Bag(contents - element)
      case Some(c) => Bag(contents.updated(element, c - 1))
    }

  def +(element: A) =
    contents.get(element) match {
      case None => Bag(contents + (element -> 1))
      case Some(c) => Bag(contents.updated(element, c + 1))
    }

  def ++(other: Bag[A]) = (contents.keySet union other.contents.keySet).map(k =>
    k -> (contents.get(k).getOrElse(0) + other.contents.get(k).getOrElse(0))).toMap

  override def iterator: Iterator[A] =
    (for {
      (e, c) <- contents
      _ <- 1 to c
    } yield e).iterator

  override def headOption: Option[A] = contents.headOption.map(_._1)
  override def head: A = contents.head._1
  override def tail: Bag[A] = this - head
  
  override def newBuilder: Builder[A, Bag[A]] = new Builder[A, Bag[A]]() {
    var target = Bag.empty[A]
    override def +=(element: A) = { target = target + element; this }
    override def clear = target = Bag.empty[A]
    override def result = target
  } 
  
  def contains(item: A) = contents.contains(item)
    
}

object Bag {
  def empty[A] = new Bag(Map.empty[A, Int])
  def apply[A](elements: Iterable[A]): Bag[A] = new Bag(elements.groupBy(identity).mapValues(_.size))
  def apply[A](first: A, rest: A*): Bag[A] = Bag(Iterable(first) ++ rest.toIterable)
}