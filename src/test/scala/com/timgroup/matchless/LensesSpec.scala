package com.timgroup.matchless

import org.specs2.Specification
import com.timgroup.matchless.utils.Lenses._
import PropertyMatchers._

class LensesSpec extends Specification {

  case class Foo(bar: Int, baz: String)
  val foo = Foo(1, "apple")
  val grinder = LensGrinder[Foo]
  val barP = grinder.projection(_.bar)
  val barR = grinder.reduction[Int]((t, v) => t.copy(bar = v))
  val bazP = grinder.projection(_.baz)
  val charAt = (i: Int) => (_: String)(i)
  val barL = (barP, barR)
  val bazL = grinder.lens(_.baz, (t, v: String) => t.copy(baz = v))

  def is =
    "Lenses are composable" ! {
      (barP(foo) must_== 1) and
        ((bazP ~ charAt(2))(foo) must_== 'p') and
        (barL.update(foo, _ + 1) must_== Foo(2, "apple")) and
        (foo must haveProperty("bar", barP, 1)) and
        (foo must haveProperty("fourth character of baz", bazP ~ charAt(3), 'l'))
    } ^
      "Readers are bindable" ! {
        val bound = bazP.bind(foo)

        bound() must_== "apple"
      } ^
      "Writers are bindable" ! {
        val bound = barR.bind(foo)

        bound(3) must_== Foo(3, "apple")
      } ^
      "Lenses are bindable" ! {
        val bound = barL.bind(foo)

        (bound() must_== 1) and
          (bound(3) must_== Foo(3, "apple")) and
          (bound.update(_ * 7) must_== Foo(7, "apple")) and
          (bound.value must_== Foo(1, "apple"))
      } ^
      "Lenses can be applied in a monad" ! {
        ((for {
          bar <- barP
          baz <- bazP
          _ <- barR := bar + 7
          _ <- bazL /= (_ + " " + bar)
        } yield None) ~> foo) must_== Foo(8, "apple 1")
      }

}