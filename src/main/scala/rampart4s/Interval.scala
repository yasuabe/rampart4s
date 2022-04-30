package rampart4s

import cats.Order
import cats.Comparison
import Relation.*

trait Interval[-P, A](using o: Order[A]):

  def lesser(p: P): A
  def greater(p: P): A
  def isEmpty(p: P): Boolean = o.compare(lesser(p), greater(p)) == 0
  def nonEmpty(p: P): Boolean = !isEmpty(p)

  def relate(x: P, y: P): Relation =
    import Comparison.{EqualTo as EQ, LessThan as LT, GreaterThan as GT}
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

    def isBefore(q: C):       Boolean = relate(q) == Before
    def meets(q: C):          Boolean = relate(q) == Meets
    def overlaps(q: C):       Boolean = relate(q) == Overlaps
    def isFinishedBy(q: C):   Boolean = relate(q) == FinishedBy
    def contains(q: C):       Boolean = relate(q) == Contains
    def starts(q: C):         Boolean = relate(q) == Starts
    def equalsTo(q: C):       Boolean = relate(q) == Equal
    def isStartedBy(q: C):    Boolean = relate(q) == StartedBy
    def isDuring(q: C):       Boolean = relate(q) == During
    def finishes(q: C):       Boolean = relate(q) == Finishes
    def isOverlappedBy(q: C): Boolean = relate(q) == OverlappedBy
    def isMetBy(q: C):        Boolean = relate(q) == MetBy
    def isAfter(q: C):        Boolean = relate(q) == After

  type Pair[A] = (A, A)
  given [A](using o: Order[A]): Interval[Pair[A], A] with
    def lesser(p: Pair[A]): A = o.min(p._1, p._2)
    def greater(p: Pair[A]): A = o.max(p._1, p._2)
  
  given Interval[Range, Int] with 
    def lesser(a: Range): Int = a.min
    def greater(a: Range): Int = a.max

  def pointInstance [A: Order]: Interval[A, A] = new Interval[A, A]:
    def lesser(a: A): A = a
    def greater(a: A): A = a
