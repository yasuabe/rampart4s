package rampart4s3

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.cats.implicits._
import cats.Apply
import Rampart.Interval

class IntervalSuite extends ScalaCheckSuite {

  val genSmallInt = Gen.chooseNum(0, 10)
  val genIntInterval =
    summon[Apply[Gen]].map2(genSmallInt, genSmallInt)(Interval(_, _))

  given Arbitrary[Interval[Int]] = Arbitrary(genIntInterval)

  property("lesser is always less than or equals to greater") {
    forAll { (interval: Interval[Int]) => interval.lesser <= interval.greater }
  }
  property("interval is empty iif lesser equals to greater") {
    given Arbitrary[Int] = Arbitrary(genSmallInt)
    forAll { (l: Int, g: Int) => Interval(l, g).isEmpty == (l == g) }
  }
  property("interval is nonEmpty iif lesser doesn't equal to greater") {
    given Arbitrary[Int] = Arbitrary(genSmallInt)
    forAll { (l: Int, g: Int) => Interval(l, g).nonEmpty == (l != g) }
  }
  property("x.lesser = y.lesser ∧ x.greater = y.greater → Equal") {
    assert((Interval(1, 2) relate Interval(1, 2)) == Relation.Equal)
    assert((Interval(2, 2) relate Interval(2, 2)) == Relation.Equal)

    forAll { (x: Interval[Int], y: Interval[Int]) =>
      val cond = x.lesser == y.lesser && x.greater == y.greater
      cond == ((x relate y) == Relation.Equal)
    }
  }
}