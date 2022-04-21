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
  property("x.nonEmpty ∧ y.nonEmpty ∧ x.greater = y.lesser→ Meets") {
    assert((Interval(1, 2) relate Interval(2, 3)) == Relation.Meets)

    forAll { (x: Interval[Int], y: Interval[Int]) =>
      val cond = x.nonEmpty && y.nonEmpty && x.greater == y.lesser
      cond == ((x relate y) == Relation.Meets)
    }
  }
  property("x.greater < y.lesser → Before") {
    assert((Interval(0, 1) relate Interval(2, 2)) == Relation.Before)
    assert((Interval(1, 1) relate Interval(2, 2)) == Relation.Before)
    assert((Interval(1, 1) relate Interval(2, 3)) == Relation.Before)
    assert((Interval(0, 1) relate Interval(2, 3)) == Relation.Before)

    forAll { (x: Interval[Int], y: Interval[Int]) =>
      val cond = x.greater < y.lesser
      cond == ((x relate y) == Relation.Before)
    }
  }
  property("x.lesser > y.greater → After") {
    assert((Interval(2, 2) relate Interval(0, 1)) == Relation.After)
    assert((Interval(2, 2) relate Interval(1, 1)) == Relation.After)
    assert((Interval(2, 3) relate Interval(1, 1)) == Relation.After)
    assert((Interval(2, 3) relate Interval(0, 1)) == Relation.After)

    forAll { (x: Interval[Int], y: Interval[Int]) =>
      val cond = x.lesser > y.greater
      cond == ((x relate y) == Relation.After)
    }
  }
  property("x.nonEmpty ∧ y.nonEmpty ∧ x.greater = y.lesser → MetBy") {
    assert((Interval(2, 3) relate Interval(1, 2)) == Relation.MetBy)

    forAll { (x: Interval[Int], y: Interval[Int]) =>
      val cond = x.nonEmpty && y.nonEmpty && x.lesser == y.greater
      cond == ((x relate y) == Relation.MetBy)
    }
  }
}