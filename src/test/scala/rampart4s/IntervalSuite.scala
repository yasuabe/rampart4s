package rampart4s

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.cats.implicits._
import cats.Apply
import Interval.*

class IntervalSuite extends ScalaCheckSuite {

  val genSmallInt    = Gen.chooseNum(0, 10)
  val genIntInterval = summon[Apply[Gen]].map2(genSmallInt, genSmallInt)((_, _))

  given Arbitrary[(Int, Int)] = Arbitrary(genIntInterval)

  given pi: Interval[(Int, Int), Int] = createPairInstance[Int]

  property("lesser is always less than or equals to greater") {
    forAll { (i: (Int, Int)) => pi.lesser(i) <= pi.greater(i) }
  }
  property("interval is empty iif lesser equals to greater") {
    given Arbitrary[Int] = Arbitrary(genSmallInt)
    forAll { (l: Int, g: Int) => (l, g).isEmpty == (l == g) }
  }
  property("interval is nonEmpty iif lesser doesn't equal to greater") {
    given Arbitrary[Int] = Arbitrary(genSmallInt)
    forAll { (l: Int, g: Int) => (l, g).nonEmpty == (l != g) }
  }
  property("x.lesser = y.lesser ∧ x.greater = y.greater → Equal") {
    assert(((1, 2) relate (1, 2)) == Relation.Equal)
    assert(((2, 2) relate (2, 2)) == Relation.Equal)

    forAll { (x: (Int, Int), y: (Int, Int)) =>
      val cond = x.lesser == y.lesser && x.greater == y.greater
      cond == ((x relate y) == Relation.Equal)
    }
  }
  property("x.nonEmpty ∧ y.nonEmpty ∧ x.greater = y.lesser→ Meets") {
    assert(((1, 2) relate (2, 3)) == Relation.Meets)

    forAll { (x: (Int, Int), y: (Int, Int)) =>
      val cond = x.nonEmpty && y.nonEmpty && x.greater == y.lesser
      cond == ((x relate y) == Relation.Meets)
    }
  }
  property("x.nonEmpty ∧ y.nonEmpty ∧ x.greater = y.lesser → MetBy") {
    assert(((2, 3) relate (1, 2)) == Relation.MetBy)

    forAll { (x: (Int, Int), y: (Int, Int)) =>
      val cond = x.nonEmpty && y.nonEmpty && x.lesser == y.greater
      cond == ((x relate y) == Relation.MetBy)
    }
  }
  property("x.nonEmpty ∧ y.nonEmpty ∧ x.lesser = y.lesser ∧ x.greater < y.greater → Starts") {
    assert(((3, 4) relate (3, 7)) == Relation.Starts)

    forAll { (x: (Int, Int), y: (Int, Int)) =>
      val cond = x.nonEmpty && y.nonEmpty && x.lesser == y.lesser && x.greater < y.greater
      cond == ((x relate y) == Relation.Starts)
    }
  }
  property("x.nonEmpty ∧ y.nonEmpty ∧ x.lesser = y.lesser ∧ x.greater > y.greater → StartedBy") {
    assert(((3, 8) relate (3, 7)) == Relation.StartedBy)

    forAll { (x: (Int, Int), y: (Int, Int)) =>
      val cond = x.nonEmpty && y.nonEmpty && x.lesser == y.lesser && x.greater > y.greater
      cond == ((x relate y) == Relation.StartedBy)
    }
  }
  property("x.greater < y.lesser → Before") {
    assert(((0, 1) relate (2, 2)) == Relation.Before)
    assert(((1, 1) relate (2, 2)) == Relation.Before)
    assert(((1, 1) relate (2, 3)) == Relation.Before)
    assert(((0, 1) relate (2, 3)) == Relation.Before)

    forAll { (x: (Int, Int), y: (Int, Int)) =>
      val cond = x.greater < y.lesser
      cond == ((x relate y) == Relation.Before)
    }
  }
  property("x.lesser > y.greater → After") {
    assert(((2, 2) relate (0, 1)) == Relation.After)
    assert(((2, 2) relate (1, 1)) == Relation.After)
    assert(((2, 3) relate (1, 1)) == Relation.After)
    assert(((2, 3) relate (0, 1)) == Relation.After)

    forAll { (x: (Int, Int), y: (Int, Int)) =>
      val cond = x.lesser > y.greater
      cond == ((x relate y) == Relation.After)
    }
  }
  property("x.nonEmpty ∧ x.lesser > y.lesser ∧ x.greater = y.greater → Finishes") {
    assert(((6, 7) relate (3, 7)) == Relation.Finishes)

    forAll { (x: (Int, Int), y: (Int, Int)) =>
      val cond = x.nonEmpty && x.lesser > y.lesser && x.greater == y.greater
      cond == ((x relate y) == Relation.Finishes)
    }
  }
  property("y.nonEmpty ∧ x.lesser < y.lesser ∧ x.greater = y.greater → FinishedBy") {
    assert(((2, 7) relate (3, 7)) == Relation.FinishedBy)

    forAll { (x: (Int, Int), y: (Int, Int)) =>
      val cond = y.nonEmpty && x.lesser < y.lesser && x.greater == y.greater
      cond == ((x relate y) == Relation.FinishedBy)
    }
  }
  property("x.lesser < y.lesser ∧ x.greater > y.greater → Contains") {
    assert(((2, 8) relate (3, 7)) == Relation.Contains)

    forAll { (x: (Int, Int), y: (Int, Int)) =>
      val cond = x.lesser < y.lesser && x.greater > y.greater
      cond == ((x relate y) == Relation.Contains)
    }
  }
  property("x.lesser > y.lesser ∧ x.greater < y.greater → During") {
    assert(((3, 7) relate (2, 8)) == Relation.During)

    forAll { (x: (Int, Int), y: (Int, Int)) =>
      val cond = x.lesser > y.lesser && x.greater < y.greater
      cond == ((x relate y) == Relation.During)
    }
  }
  property("other x < y patterns  → Overlaps") {
    assert(((2, 4) relate (3, 7)) == Relation.Overlaps)
    assert(((3, 3) relate (3, 7)) == Relation.Overlaps)
    assert(((3, 7) relate (7, 7)) == Relation.Overlaps)

    forAll { (x: (Int, Int), y: (Int, Int)) =>
      val cond = x.lesser < y.lesser && x.greater > y.lesser && x.greater < y.greater
              || x.isEmpty != y.isEmpty && x.lesser < y.greater && x.greater == y.lesser
      cond == ((x relate y) == Relation.Overlaps)
    }
  }
  property("other x > y patterns  → OverlappedBy") {
    assert(((3, 7) relate (2, 4)) == Relation.OverlappedBy)
    assert(((3, 7) relate (3, 3)) == Relation.OverlappedBy)
    assert(((7, 7) relate (3, 7)) == Relation.OverlappedBy)

    forAll { (x: (Int, Int), y: (Int, Int)) =>
      val cond = x.lesser > y.lesser && x.lesser < y.greater && x.greater > y.greater
              || x.isEmpty != y.isEmpty && x.greater > y.lesser &&  x.lesser == y.greater
      cond == ((x relate y) == Relation.OverlappedBy)
    }
  }
  property("(x relate y).invert = y relate x") {
    forAll { (x: (Int, Int), y: (Int, Int)) =>
      (x relate y).invert == (y relate x)
    }
  }
  property("(a relate b) == ((a, a) relate (b, b))") {
    given Interval[Int, Int] = createSingleValInterval[Int]

    forAll { (a: Int, b: Int) =>
      (a relate b) == ((a, a) relate (b, b))
    }
  }
  property("(a relate b) in {BEFORE, EQUAL, AFTER}") {
    given Interval[Int, Int] = createSingleValInterval[Int]
    val rs = Set(Relation.Before, Relation.Equal, Relation.After)

    forAll { (a: Int, b: Int) => rs.contains(a relate b) }
  }
  property("works for non-empty Range") {
    given Arbitrary[Range] = Arbitrary(
      genIntInterval.map[Range](Range.apply(_, _)).suchThat(_.nonEmpty)
    )
    forAll { (a: Range, b: Range) =>
      (a relate b).invert == (b relate a)
    }
  }
  property("has predicate per relation") {
    assert((1, 2) isBefore       (3, 7))
    assert((2, 3) meets          (3, 7))
    assert((2, 4) overlaps       (3, 7))
    assert((2, 7) isFinishedBy   (3, 7))
    assert((2, 8) contains       (3, 7))
    assert((3, 4) starts         (3, 7))
    assert((3, 7) equalsTo       (3, 7))
    assert((3, 8) isStartedBy    (3, 7))
    assert((4, 6) isDuring       (3, 7))
    assert((6, 7) finishes       (3, 7))
    assert((6, 8) isOverlappedBy (3, 7))
    assert((7, 8) isMetBy        (3, 7))
    assert((8, 9) isAfter        (3, 7))

    assert((3, 3) overlaps       (3, 7))
    assert((7, 7) isOverlappedBy (3, 7))
    assert((3, 7) isOverlappedBy (3, 3))
    assert((3, 7) overlaps       (7, 7))

    forAll { (x: (Int, Int), y: (Int, Int)) =>
      (x isBefore       y) == (y isAfter        x)
      (x meets          y) == (y isMetBy        x)
      (x overlaps       y) == (y isOverlappedBy x)
      (x isFinishedBy   y) == (y finishes       x)
      (x contains       y) == (y isDuring       x)
      (x starts         y) == (y isStartedBy    x)
      (x equalsTo       y) == (y equalsTo       x)
    }
  }
}