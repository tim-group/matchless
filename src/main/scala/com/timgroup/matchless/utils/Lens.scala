package com.timgroup.matchless.utils

import States._

object Lenses {
  
  type Projection[A, B] = A => B
  type Projectable[A] = () => A
  type Reduction[A, B] = (A, B) => A
  type Reducible[A, B] = B => A
  type Update[A] = A => A
  
  sealed case class EnrichedProjection[A, B](projection: Projection[A, B]) {
    def >>[C](other: Projection[B, C]): Projection[A, C] = projection andThen other
    def bind(target: A): Projectable[B] = () => projection(target)
    def reducingWith(reduction: Reduction[A, B]): Lens[A, B] = Lens(projection, reduction)
  }

  implicit def projection2EnrichedProjection[A, B](projection: A => B): EnrichedProjection[A, B] =
    EnrichedProjection(projection)

  sealed case class EnrichedReduction[A, B](reduction: Reduction[A, B]) {
    def projectingWith(projection: Projection[A, B]): Lens[A, B] = Lens(projection, reduction)
    def bind(target: A): Reducible[A, B] = (newValue) => reduction(target, newValue)
    def :=(value: B) = state((s: A) => (reduction(s, value), ()))
  }

  implicit def reduction2EnrichedReduction[A, B](reduction: (A, B) => A) = EnrichedReduction[A, B](reduction)

  implicit def projectionAndReduction2Lens[A, B](tuple: (Projection[A, B], Reduction[A, B])): Lens[A, B] =
    Lens(tuple._1, tuple._2)

  sealed case class Lens[A, B](projection: Projection[A, B], reduction: Reduction[A, B])
    extends Projection[A, B] with Reduction[A, B] {

    override def apply(a: A): B = projection(a)
    override def apply(target: A, newValue: B): A = reduction(target, newValue)

    def update(target: A, update: Update[B]): A = reduction(target, update(projection(target)))

    def >>[C](other: Projection[B, C]): Projection[A, C] = projection >> other
    def >>[C](reduction: Reduction[B, C]): Reduction[A, C] = (target: A, newValue: C) =>
      update(target, target2 => reduction(target2, newValue))

    def >>[C](lens: Lens[B, C]): Lens[A, C] = (this >> lens.projection, this >> lens.reduction)

    def bind(target: A): Cell[A, B] = Cell(target, this)
    
    def :=(value: B) = state((s: A) => (reduction(s, value), ()))
    def /=(updater: Update[B]) = state((s: A) => (update(s, updater), ()))
  }
  
  implicit def lens2[A, B, C](lenses: (Lens[A, B], Lens[A, C])): Lens[A, (B, C)] = Lens[A, (B, C)](
    s      => (lenses._1(s), lenses._2(s)),
    (s, v) => (lenses._2(lenses._1(s, v._1), v._2))
  )
  
  implicit def lens3[A, B, C, D](lenses: (Lens[A, B], Lens[A, C], Lens[A, D])): Lens[A, (B, C, D)] = Lens[A, (B, C, D)](
    s      => (lenses._1(s), lenses._2(s), lenses._3(s)),
    (s, v) => (lenses._3(lenses._2(lenses._1(s, v._1), v._2), v._3))
  )
  
  implicit def lens3[A, B, C, D, E](lenses: (Lens[A, B], Lens[A, C], Lens[A, D], Lens[A, E])): Lens[A, (B, C, D, E)] = Lens[A, (B, C, D, E)](
    s      => (lenses._1(s), lenses._2(s), lenses._3(s), lenses._4(s)),
    (s, v) => (lenses._4(lenses._3(lenses._2(lenses._1(s, v._1), v._2), v._3), v._4))
  )
  
  implicit def projection2[A, B, C](projections: (Projection[A, B], Projection[A, C])): Projection[A, (B, C)] =
    s      => (projections._1(s), projections._2(s))
    
  implicit def projection3[A, B, C, D](projections: (Projection[A, B], Projection[A, C], Projection[A, D])): Projection[A, (B, C, D)] =
    s      => (projections._1(s), projections._2(s), projections._3(s))
    
  implicit def projection4[A, B, C, D, E](projections: (Projection[A, B], Projection[A, C], Projection[A, D], Projection[A, E])): Projection[A, (B, C, D, E)] =
    s      => (projections._1(s), projections._2(s), projections._3(s), projections._4(s))
  
  implicit def projection2State[A, B](projection: A => B): State[A, B] = state((s: A) => (s, projection(s)))
  
  sealed case class Cell[A, B](target: A, lens: Lens[A, B]) extends Projectable[B] with Reducible[A, B] {
    override def apply() = lens(target)
    override def apply(newValue: B) = lens(target, newValue)
    def update(update: Update[B]): A = lens.update(target, update)
    def value = update(identity)
  }
  
  sealed case class LensGrinder[A]() {
    def projection[B](projection: Projection[A, B]) = projection
    def reduction[B](reduction: Reduction[A, B]) = reduction
    def lens[B](projection: Projection[A, B], reduction: Reduction[A, B]): Lens[A, B] = Lens(projection, reduction)
  }
  
  def itemL[A](index: Int): Lens[Seq[A], A] = Lens[Seq[A], A](_(index), (s, v) => s.zipWithIndex.map {
    case (a, i) => if (i == index) v else a
  })
  
  def valueForL[K, V](key: K): Lens[Map[K, V], V] = Lens[Map[K, V], V](_(key), (s, v) => s + (key -> v))
  
  def predL[A](pred: Function[A, Boolean]): Lens[Traversable[A], A] = Lens[Traversable[A], A](
      s      => s.find(pred).get,
      (s, v) => s.map(i => if (pred(i)) v else i)
  )

}