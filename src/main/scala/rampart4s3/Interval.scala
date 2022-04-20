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
