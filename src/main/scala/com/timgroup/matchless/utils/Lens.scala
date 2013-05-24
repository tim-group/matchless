package com.timgroup.matchless.utils

object Lenses {
  trait Bindable[A, B] {
    def bind(target: A): B
  }
  
  trait Reader[A, B] extends Function[A, B] {
    def ~[C](reader: Reader[B, C]): BindableReader[A, C] = reader compose this
  }
  
  type Readable[A] = () => A
  
  trait BindableReader[A, B] extends Reader[A, B] with Bindable[A, Readable[B]]
  
  private[this] case class FunctionReader[A, B](f: A => B) extends BindableReader[A, B] {
    override def apply(target: A): B = f(target)
    override def bind(target: A): Readable[B] = () => this(target)
  }
  
  implicit def readerFunction2Reader[A, B](f: A => B): BindableReader[A, B] =
    FunctionReader[A, B](f)

  trait Writer[A, B] extends Function2[A, B, A] {
    def readingWith(reader: Reader[A, B]): Lens[A, B] = Lens(reader, this)
  }
  
  type Writable[A, B] = B => A
  
  trait BindableWriter[A, B] extends Writer[A, B] with Bindable[A, Writable[A, B]]
    
  private[this] case class FunctionWriter[A, B](f: (A, B) => A) extends BindableWriter[A, B] {
    override def apply(target: A, newValue: B): A = f(target, newValue)
    override def bind(target: A): Writable[A, B] = (newValue) => this(target, newValue)
  }
  
  implicit def writerFunction2Writer[A, B](f: (A, B) => A): BindableWriter[A, B] =
    FunctionWriter[A, B](f)

  implicit def lensFunctions2Lens[A, B](tuple: (A => B, (A, B) => A)): Lens[A, B] = Lens(tuple._1, tuple._2)

  trait Cell[A, B] extends Readable[B] with Writable[A, B] {
    def update(f: B => B): A = this(f(this.apply))
    def value = update(identity)
  }
  
  trait Updater[A, B] { self: Reader[A, B] with Writer[A, B] =>
    def update(target: A, f: B => B): A = this(target, f(this(target)))
  }
  
  case class Lens[A, B](reader: Reader[A, B], writer: Writer[A, B]) extends Reader[A, B] with Writer[A, B] with Updater[A, B] with Bindable[A, Cell[A, B]] {
    override def apply(a: A): B = reader(a)
    override def apply(target: A, newValue: B): A = writer(target, newValue)

    def ~[C](writer: Writer[B, C]): BindableWriter[A, C] = (target: A, newValue: C) =>
      update(target, target2 => writer(target2, newValue))
      
    def ~[C](lens: Lens[B, C]): Lens[A, C] = (this ~ lens.reader, this ~ lens.writer)
    
    override def bind(target: A): Cell[A, B] = new Cell[A, B] {
      override def apply() = reader(target)
      override def apply(newValue: B) = writer(target, newValue)
    }
  }
  
  case class LensGrinder[A]() {
    def reader[B](f: A => B): BindableReader[A, B] = f
    def writer[B](f: (A, B) => A): BindableWriter[A, B] = f
    def lens[B](reader: A => B, writer: (A, B) => A): Lens[A, B] = (reader, writer)
  }

}