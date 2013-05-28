package com.timgroup.matchless

import org.specs2.Specification
import com.timgroup.matchless.utils.Lenses._
import PropertyMatchers._

class LensesSpec extends Specification {

  case class Foo(bar: Int, baz: String)
  val foo = Foo(1, "apple")
  val grinder = LensGrinder[Foo]
  val barReader = grinder.projection(_.bar)
  val barWriter = grinder.reduction[Int]((t, v) => t.copy(bar = v))
  val bazReader = grinder.projection(_.baz)
  val charAt = (i:Int) => (_:String)(i)  
  val barLens = (barReader, barWriter)
  val bazLens = grinder.lens(_.baz, (t, v: String) => t.copy(baz = v))
  
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
  } ^
  "Lenses can be applied in a monad" ! {
    val fooUpdater = for {
      bar <- barReader.?
      baz <- bazReader.?
      _   <- barWriter := bar + 7
      _   <- bazLens /= (_ + " " + bar)
      } yield None
      
      (fooUpdater ~> foo) must_== Foo(8, "apple 1")
   }

}