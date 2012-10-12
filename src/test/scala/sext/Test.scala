package sext

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Test extends FunSuite with ShouldMatchers {
  import Test._
  test("zipBy preserves root type"){
    Seq(1,2,3).zipBy(_ + 3) should beInstanceOf[Seq[_]]
    Seq(1,2,3).zipBy(_ + 3) should not (beInstanceOf[Set[_]])
  }
  test("mapKeys") {
    Map("b" -> 1, "c" -> 4, "a" -> 9).mapKeys(_ + "1") should
    equal(Map("b1" -> 1, "c1" -> 4, "a1" -> 9))
  }
  test("toInstanceOf returns None for unmatching type"){
    8.toInstanceOf[String] should equal(None)
  }
  test("toInstanceOf returns Some for a matching type"){
    8.toInstanceOf[Int] should equal(Some(8))
  }
  test("toInstanceOf returns Some for an inheriting type"){
    List(1).toInstanceOf[Seq[Int]] should be ('defined)
  }
  test("toInstanceOf on mixins"){
    (new A with B).toInstanceOf[B] should be ('defined)
  }
  test("toInstanceOf on general types as input"){
    (Seq(new A with B, new A{}, new B {}))
      .flatMap{_.toInstanceOf[B]}
      .should( have size (2) )
  }
}
object Test {
  trait A
  trait B

  import org.scalatest.matchers._
  import reflect._
  def beInstanceOf [A : ClassTag]
    = Matcher { x : Any =>
        MatchResult(
          classTag[A].runtimeClass.isAssignableFrom(x.getClass),
          x.getClass + " is not an instance of " + classTag[A].runtimeClass,
          x.getClass + " is an instance of " + classTag[A].runtimeClass
        )
      }

}