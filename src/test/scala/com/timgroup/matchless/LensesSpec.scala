package com.timgroup.matchless

import org.specs2.Specification
import com.timgroup.matchless.utils.Lenses._
import PropertyMatchers._

class LensesSpec extends Specification {

  case class Foo(bar: Int, baz: String)
  val foo = Foo(1, "apple")
  val grinder = LensGrinder[Foo]
  val barReader = grinder.reader(_.bar)
  val barWriter = grinder.writer[Int]((t, v) => t.copy(bar = v))
  val bazReader = grinder.reader(_.baz)
  val charAt = (i:Int) => (_:String)(i)  
  val barLens = (barReader, barWriter) 
  
  def is =
  "Lenses are composable" ! {
    (barReader(foo) must_== 1) and
    ((bazReader ~ charAt(2))(foo) must_== 'p') and
    (barLens.update(foo, _ + 1) must_== Foo(2, "apple")) and    
    (foo must haveProperty("bar", barReader, 1)) and
    (foo must haveProperty("fourth character of baz", bazReader ~ charAt(3), 'l'))
  } ^
  "Readers are bindable" ! {
    val bound = bazReader.bind(foo)
    
    bound() must_== "apple"
  } ^
  "Writers are bindable" ! {
    val bound = barWriter.bind(foo)
    
    bound(3) must_== Foo(3, "apple")
  } ^
  "Lenses are bindable" ! {
    val bound = barLens.bind(foo)
    
    (bound() must_== 1) and
    (bound(3) must_== Foo(3, "apple")) and
    (bound.update(_ * 7) must_== Foo(7, "apple")) and
    (bound.value must_== Foo(1, "apple"))
  }
  
}