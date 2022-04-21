package rampart4s3

import cats.Order
import cats.Comparison

object Rampart:
  opaque type Interval[A] = (A, A)

  object Interval:
    def apply[A](x: A, y: A)(using o: Order[A]): Interval[A] = (o.min(x, y), o.max(x, y))
  
  extension [A: Order](i: Interval[A])
    def lesser: A = i._1
    def greater: A = i._2
    def isEmpty: Boolean = i._1 == i._2
    def nonEmpty: Boolean = !isEmpty
    def relate(y: Interval[A]): Relation =
      import Comparison.{EqualTo as EQ, LessThan as LT, GreaterThan as GT}
      import Relation.*
      def c(a: A, b: A) = summon[Order[A]].comparison(a, b)
      ( c(i.lesser,  y.lesser),
        c(i.lesser,  y.greater),
        c(i.greater, y.lesser),
        c(i.greater, y.greater)) match
      case (EQ, _, _ , EQ) => Equal
      case (LT, _, EQ, _ ) => Meets 
      case (_,  _, _,  _) => OverlappedBy
