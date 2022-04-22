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
  property("x.nonEmpty ∧ y.nonEmpty ∧ x.greater = y.lesser → MetBy") {
    assert((Interval(2, 3) relate Interval(1, 2)) == Relation.MetBy)

    forAll { (x: Interval[Int], y: Interval[Int]) =>
      val cond = x.nonEmpty && y.nonEmpty && x.lesser == y.greater
      cond == ((x relate y) == Relation.MetBy)
    }
  }
  property("x.nonEmpty ∧ y.nonEmpty ∧ x.lesser = y.lesser ∧ x.greater < y.greater → Starts") {
    assert((Interval(3, 4) relate Interval(3, 7)) == Relation.Starts)

    forAll { (x: Interval[Int], y: Interval[Int]) =>
      val cond = x.nonEmpty && y.nonEmpty && x.lesser == y.lesser && x.greater < y.greater
      cond == ((x relate y) == Relation.Starts)
    }
  }
  property("x.nonEmpty ∧ y.nonEmpty ∧ x.lesser = y.lesser ∧ x.greater > y.greater → StartedBy") {
    assert((Interval(3, 8) relate Interval(3, 7)) == Relation.StartedBy)

    forAll { (x: Interval[Int], y: Interval[Int]) =>
      val cond = x.nonEmpty && y.nonEmpty && x.lesser == y.lesser && x.greater > y.greater
      cond == ((x relate y) == Relation.StartedBy)
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
  property("x.nonEmpty ∧ x.lesser > y.lesser ∧ x.greater = y.greater → Finishes") {
    assert((Interval(6, 7) relate Interval(3, 7)) == Relation.Finishes)

    forAll { (x: Interval[Int], y: Interval[Int]) =>
      val cond = x.nonEmpty && x.lesser > y.lesser && x.greater == y.greater
      cond == ((x relate y) == Relation.Finishes)
    }
  }
  property("y.nonEmpty ∧ x.lesser < y.lesser ∧ x.greater = y.greater → FinishedBy") {
    assert((Interval(2, 7) relate Interval(3, 7)) == Relation.FinishedBy)

    forAll { (x: Interval[Int], y: Interval[Int]) =>
      val cond = y.nonEmpty && x.lesser < y.lesser && x.greater == y.greater
      cond == ((x relate y) == Relation.FinishedBy)
    }
  }
  property("x.lesser < y.lesser ∧ x.greater > y.greater → Contains") {
    assert((Interval(2, 8) relate Interval(3, 7)) == Relation.Contains)

    forAll { (x: Interval[Int], y: Interval[Int]) =>
      val cond = x.lesser < y.lesser && x.greater > y.greater
      cond == ((x relate y) == Relation.Contains)
    }
  }
  property("x.lesser > y.lesser ∧ x.greater < y.greater → During") {
    assert((Interval(3, 7) relate Interval(2, 8)) == Relation.During)

    forAll { (x: Interval[Int], y: Interval[Int]) =>
      val cond = x.lesser > y.lesser && x.greater < y.greater
      cond == ((x relate y) == Relation.During)
    }
  }
  property("other x < y patterns  → Overlaps") {
    assert((Interval(2, 4) relate Interval(3, 7)) == Relation.Overlaps)
    assert((Interval(3, 3) relate Interval(3, 7)) == Relation.Overlaps)
    assert((Interval(3, 7) relate Interval(7, 7)) == Relation.Overlaps)

    forAll { (x: Interval[Int], y: Interval[Int]) =>
      val cond = x.lesser < y.lesser && x.greater > y.lesser && x.greater < y.greater
               || x.isEmpty && y.nonEmpty && x.lesser == y.lesser && x.greater == y.lesser
               || x.nonEmpty && y.isEmpty && x.greater == y.lesser && x.greater == y.greater
      cond == ((x relate y) == Relation.Overlaps)
    }
  }
  property("other x > y patterns  → OverlappedBy") {
    assert((Interval(3, 7) relate Interval(2, 4)) == Relation.OverlappedBy)
    assert((Interval(3, 7) relate Interval(3, 3)) == Relation.OverlappedBy)
    assert((Interval(7, 7) relate Interval(3, 7)) == Relation.OverlappedBy)

    forAll { (x: Interval[Int], y: Interval[Int]) =>
      val cond = x.lesser > y.lesser && x.lesser < y.greater && x.greater > y.greater
               || x.isEmpty && y.nonEmpty && x.lesser == y.greater && x.greater == y.greater
               || x.nonEmpty && y.isEmpty && x.lesser == y.lesser && x.greater > y.greater
      cond == ((x relate y) == Relation.OverlappedBy)
    }
  }
  property("(x relate y).invert = y relate x") {
    forAll { (x: Interval[Int], y: Interval[Int]) =>
      (x relate y).invert == (y relate x)
    }
  }
}