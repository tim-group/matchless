package com.timgroup.matchless.utils

import States._

object Lenses {
  
  type Projection[A, B] = A => B
  type Projectable[A] = () => A
  type Reduction[A, B] = (A, B) => A
  type Reducible[A, B] = B => A
  type Update[A] = A => A
  
  sealed case class EnrichedProjection[A, B](projection: Projection[A, B]) {
    def ~[C](other: Projection[B, C]): Projection[A, C] = projection andThen other
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

    def ~[C](other: Projection[B, C]): Projection[A, C] = projection ~ other
    def ~[C](reduction: Reduction[B, C]): Reduction[A, C] = (target: A, newValue: C) =>
      update(target, target2 => reduction(target2, newValue))

    def ~[C](lens: Lens[B, C]): Lens[A, C] = (this ~ lens.projection, this ~ lens.reduction)
    def *[C](lens: Lens[A, C]): Lens2[A, B, C] = Lens2[A, B, C](this, lens)

    def bind(target: A): Cell[A, B] = Cell(target, this)
    
    def :=(value: B) = state((s: A) => (reduction(s, value), ()))
    def /=(updater: Update[B]) = state((s: A) => (update(s, updater), ()))
  }
  
  trait Lensable[A, B] {
    def toLens: Lens[A, B]
  }
  
  sealed case class Lens2[A, B, C](left: Lens[A, B], right: Lens[A, C]) extends Lensable[A, (B, C)] {
    def *[D](lens: Lens[A, D]): Lens3[A, B, C, D] = Lens3[A, B, C, D](left, right, lens)
    override def toLens: Lens[A, (B, C)] = Lens[A, (B, C)](
      s      => (left(s), right(s)),
      (s, v) => (right(left(s, v._1), v._2))
    )
  }
  
  sealed case class Lens3[A, B, C, D](left: Lens[A, B], middle: Lens[A, C], right: Lens[A, D]) extends Lensable[A, (B, C, D)] {
   def *[E](lens: Lens[A, E]): Lens4[A, B, C, D, E] = Lens4[A, B, C, D, E](left, middle, right, lens)
   override def toLens: Lens[A, (B, C, D)] = Lens[A, (B, C, D)](
      s      => (left(s), middle(s), right(s)),
      (s, v) => (right(middle(left(s, v._1), v._2), v._3))
    )
  }
  
  sealed case class Lens4[A, B, C, D, E](lens1: Lens[A, B], lens2: Lens[A, C], lens3: Lens[A, D], lens4: Lens[A, E])
   extends Lensable[A, (B, C, D, E)] {
   override def toLens: Lens[A, (B, C, D, E)] = Lens[A, (B, C, D, E)](
      s      => (lens1(s), lens2(s), lens3(s), lens4(s)),
      (s, v) => (lens4(lens3(lens2(lens1(s, v._1), v._2), v._3), v._4))
    )
  }
  
  implicit def LensableToLens[A, B](lensable: Lensable[A, B]): Lens[A, B] = lensable.toLens 
      
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