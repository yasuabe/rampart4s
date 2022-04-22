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
      def compare(a: A, b: A) = summon[Order[A]].comparison(a, b)

      ( compare(x.lesser , y.lesser ),
        compare(x.lesser , y.greater),
        compare(x.greater, y.lesser ),
        compare(x.greater, y.greater)) match
        case (_ , _ , LT, _ ) => Before
        case (_ , GT, _ , _ ) => After
        case (EQ, _ , _ , EQ) => Equal
        case (LT, _ , _ , GT) => Contains
        case (GT, _ , _ , LT) => During
        case (LT, _ , EQ, LT) => Meets 
        case (LT,  _, _ , LT) => Overlaps
        case (LT, _ , GT, EQ) => FinishedBy
        case (EQ, _ , GT, LT) => Starts
        case (EQ, LT, _ , GT) => StartedBy
        case (GT, LT, _ , EQ) => Finishes
        case (GT, EQ, _ , GT) => MetBy 
        case (GT, _ , _ , GT) => OverlappedBy
        case (_ , _ , EQ, _ ) => Overlaps
        case (_ , EQ, _ , _ ) => OverlappedBy