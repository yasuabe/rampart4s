package rampart4s

import cats.Order
import cats.Comparison
import Relation.*

trait Interval[-P, A](using o: Order[A]):

  def lesser(p: P):  A
  def greater(p: P): A

  def isEmpty(p: P):  Boolean = o.compare(lesser(p), greater(p)) == 0
  def nonEmpty(p: P): Boolean = !isEmpty(p)

object Interval:
  extension [P, A](p: P)(using i: Interval[P, A], o: Order[A])
    def isEmpty:  Boolean = i isEmpty p
    def nonEmpty: Boolean = !isEmpty
    def lesser:   A       = i lesser p
    def greater:  A       = i greater p

    def relate(q: P): Relation  = Relation(p, q)

    private def predicate(r: Relation): P => Boolean = relate(_)  == r

    def isBefore:       P => Boolean = predicate(Before)
    def meets:          P => Boolean = predicate(Meets)
    def overlaps:       P => Boolean = predicate(Overlaps)
    def isFinishedBy:   P => Boolean = predicate(FinishedBy)
    def contains:       P => Boolean = predicate(Contains)
    def starts:         P => Boolean = predicate(Starts)
    def equalsTo:       P => Boolean = predicate(Equal)
    def isStartedBy:    P => Boolean = predicate(StartedBy)
    def isDuring:       P => Boolean = predicate(During)
    def finishes:       P => Boolean = predicate(Finishes)
    def isOverlappedBy: P => Boolean = predicate(OverlappedBy)
    def isMetBy:        P => Boolean = predicate(MetBy)
    def isAfter:        P => Boolean = predicate(After)

  type Pair[A] = (A, A)
  given [A](using o: Order[A]): Interval[Pair[A], A] with
    def lesser(p: Pair[A]): A = o.min(p._1, p._2)
    def greater(p: Pair[A]): A = o.max(p._1, p._2)
  
  given Interval[Range, Int] with 
    def lesser(r: Range): Int = r.min
    def greater(r: Range): Int = r.max

  def pointInstance [A: Order]: Interval[A, A] = new Interval[A, A]:
    def lesser(a: A): A = a
    def greater(a: A): A = a
