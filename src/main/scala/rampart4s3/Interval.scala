package rampart4s3

import cats.Order
import cats.Comparison

object Rampart:
  opaque type Interval[A] = (A, A)

  object Interval:
    def apply[A](x: A, y: A)(using o: Order[A]): Interval[A] = (o.min(x, y), o.max(x, y))
  
  extension [A: Order](x: Interval[A])
    def lesser: A = x._1
    def greater: A = x._2
    def isEmpty: Boolean = x._1 == x._2
    def nonEmpty: Boolean = !isEmpty
    def relate(y: Interval[A]): Relation =
      import Comparison.{EqualTo as EQ, LessThan as LT, GreaterThan as GT}
      import Relation.*
      def c(a: A, b: A) = summon[Order[A]].comparison(a, b)
      ( c(x.lesser,  y.lesser),
        c(x.lesser,  y.greater),
        c(x.greater, y.lesser),
        c(x.greater, y.greater)) match
      case (EQ, _ , _ , EQ) => Equal
      case (_ , _ , LT, _ ) => Before
      case (_ , GT, _ , _ ) => After
      case (LT, _ , EQ, _ ) => Meets 
      case (_ ,  _, _ ,  _) => Dummy
