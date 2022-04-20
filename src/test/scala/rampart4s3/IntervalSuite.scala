package rampart4s3

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.cats.implicits._
import cats.Apply
import Rampart.Interval

class IntervalSuite extends ScalaCheckSuite {

  val genSmallInt = Gen.chooseNum(-10, 10)
  val genIntInterval =
    summon[Apply[Gen]].map2(genSmallInt, genSmallInt)(Interval(_, _))

  given Arbitrary[Interval[Int]] = Arbitrary(genIntInterval)

  property("lesser is always less than or equals to greater") {
    forAll { (interval: Interval[Int]) => interval.lesser <= interval.greater }
  }
  property("interval is empty iif lesser equals to greater") {
    given Arbitrary[Int] = Arbitrary(genSmallInt)
    forAll { (l: Int, g: Int) =>
      val i = Interval(l, g)
      Interval(l, g).isEmpty == (l == g)
    }
  }
}