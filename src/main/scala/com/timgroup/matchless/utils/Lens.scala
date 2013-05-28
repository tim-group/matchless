package com.timgroup.matchless.utils

import States._

object Lenses {
  
  type Projection[A, B] = A => B
  type Projectable[A] = () => A
  type Reduction[A, B] = (A, B) => A
  type Reducible[A, B] = B => A
  type Update[A] = A => A
  type State[S, A] = S => (S, A)
  
  sealed case class EnrichedProjection[A, B](projection: Projection[A, B]) {
    def ~[C](other: Projection[B, C]): Projection[A, C] = projection andThen other
    def bind(target: A): Projectable[B] = () => projection(target)
    def reducingWith(reduction: Reduction[A, B]): Lens[A, B] = Lens(projection, reduction)
    def ? = state((s: A) => (s, projection(s)))
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

    def ~[C](other: Projection[B, C]): Projection[A, C] = projection ~ other
    def ~[C](reduction: Reduction[B, C]): Reduction[A, C] = (target: A, newValue: C) =>
      update(target, target2 => reduction(target2, newValue))

    def ~[C](lens: Lens[B, C]): Lens[A, C] = (this ~ lens.projection, this ~ lens.reduction)

    def bind(target: A): Cell[A, B] = Cell(target, this)
    
    def ? = state((s: A) => (s, projection(s)))
    def :=(value: B) = state((s: A) => (reduction(s, value), ()))
    def /=(updater: Update[B]) = state((s: A) => (update(s, updater), ()))
  }
  
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

}