package rampart4s3

import cats.Order
import cats.Comparison

trait Interval[P, A](using o: Order[A]):
  def lesser(p: P): A
  def greater(p: P): A
  def isEmpty(p: P): Boolean = o.compare(lesser(p), greater(p)) == 0
  def nonEmpty(p: P): Boolean = !isEmpty(p)

  def relate(x: P, y: P): Relation =
    import Comparison.{EqualTo as EQ, LessThan as LT, GreaterThan as GT}
    import Relation.*
    def compare(a: A, b: A) = o.comparison(a, b)

    ( compare(lesser(x),  lesser(y) ),
      compare(lesser(x),  greater(y)),
      compare(greater(x), lesser(y) ),
      compare(greater(x), greater(y))) match
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

object Interval:
  extension [C, A](p: C)(using pi: Interval[C, A])
    def isEmpty:  Boolean = pi isEmpty p
    def nonEmpty: Boolean = !isEmpty
    def lesser:   A       = pi lesser p
    def greater:  A       = pi greater p

    def relate(q: C): Relation  = pi.relate(p, q)

  type Pair[A] = (A, A)
  def createPairInstance[A](using o: Order[A]): Interval[Pair[A], A] = new Interval[Pair[A], A]:
    def lesser(p: Pair[A]): A = o.min(p._1, p._2)
    def greater(p: Pair[A]): A = o.max(p._1, p._2)