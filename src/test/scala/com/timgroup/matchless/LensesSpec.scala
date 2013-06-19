package com.timgroup.matchless

import org.specs2.Specification
import com.timgroup.matchless.utils.Lenses._
import PropertyMatchers._
import Composition._

class LensesSpec extends Specification {

  case class Foo(bar: Int, baz: String)
  val foo = Foo(1, "apple")
  val grinder = LensGrinder[Foo]
  val barP = grinder.projection(_.bar)
  val barR = grinder.reduction[Int]((t, v) => t.copy(bar = v))
  val bazP = grinder.projection(_.baz)
  val charAt = (i: Int) => (_: String)(i)
  val barL = Lens(barP, barR)
  val bazL = grinder.lens[String](_.baz, (t, v) => t.copy(baz = v))

  def is =
    "Lenses are composable" ! {
      (barP(foo) must_== 1) and
        ((bazP >> charAt(2))(foo) must_== 'p') and
        (barL.update(foo, _ + 1) must_== Foo(2, "apple")) and
        (foo must haveProperty("bar", barP, 1)) and
        (foo must haveProperty("fourth character of baz", bazP >> charAt(3), 'l'))
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
      } ^
      "Lens monads compose" ! {
        val combined = (for {
          i <- barL
          aIsFor <- (bazP >> charAt(1))
        } yield aIsFor + i.toString) ! _

        (for {
          c <- combined
          baz <- bazL
        } yield (c, baz)) ! foo must_== ("p1", "apple")
      } ^
      "Lenses have products" ! {
        val fooAndBazL = (barL, bazL)
        (fooAndBazL(foo) must_== (1, "apple")) and
          (fooAndBazL(foo, (2, "pear")) must_== Foo(2, "pear"))
      } ^
      "Lenses have 3-products" ! {
        case class ABC(a: String, b: String, c: String)
        val aL = Lens[ABC, String](_.a, (s, v) => s.copy(a = v))
        val bL = Lens[ABC, String](_.b, (s, v) => s.copy(b = v))
        val cL = Lens[ABC, String](_.c, (s, v) => s.copy(c = v))
        val abcL = (aL, bL, cL)

        val abc = ABC("a", "b", "c")
        (abcL(abc) must_== ("a", "b", "c")) and
          (abcL(abc, ("d", "e", "f")) must_== ABC("d", "e", "f"))
      } ^
      "Lenses have 4-products" ! {
        case class ABCD(a: String, b: String, c: String, d: String)
        val aL = Lens[ABCD, String](_.a, (s, v) => s.copy(a = v))
        val bL = Lens[ABCD, String](_.b, (s, v) => s.copy(b = v))
        val cL = Lens[ABCD, String](_.c, (s, v) => s.copy(c = v))
        val dL = Lens[ABCD, String](_.d, (s, v) => s.copy(d = v))
        val abcdL = (aL, bL, cL, dL)

        val abcd = ABCD("a", "b", "c", "d")
        (abcdL(abcd) must_== ("a", "b", "c", "d")) and
          (abcdL(abcd, ("e", "f", "g", "h")) must_== ABCD("e", "f", "g", "h"))
      } ^
      "itemL is a lens into any sequence" ! {
        val thirdL = itemL[Int](2)
        allOf(
          thirdL(List(1, 2, 3, 4)) must_== 3,
          thirdL(Seq(1, 2, 3, 4)) must_== 3,
          thirdL(Stream(1, 2, 3, 4)) must_== 3,
          thirdL(Stream(1, 2, 3, 4), 7) must_== Stream(1, 2, 7, 4)
        )
      } ^
      "valueForL is a lens into any map" ! {
        val fooL = valueForL[String, Int]("foo")
        val map = Map("foo" -> 23, "bar" -> 42)
        allOf(
          fooL(map) must_== 23,
          fooL(map, 74) must_== Map("foo" -> 74, "bar" -> 42)
        )
      } ^
      "predL is a lens into any traversable" ! {
        val nonZeroL = predL[Int](_ != 0)
        val values = Set(0, 1)
        allOf(
          nonZeroL(values) must_== 1,
          nonZeroL(values, 2) must_== Set(0, 2)
        )
      }
}