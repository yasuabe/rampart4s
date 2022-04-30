## Rampart implemented in Scala 3

[Maintaining Knowledge About Temporal Intervals](https://urresearch.rochester.edu/institutionalPublicationPublicView.action?institutionalItemId=10115&versionNumber=1) -> [haskell rampart](https://hackage.haskell.org/package/rampart-2.0.0.0/docs/Rampart.html) -> this

Implemented as type class.

### Usage
```
scala> import rampart4s.Interval
scala> import Interval.{*, given} 

scala> (10, 10).isEmpty
val res0: Boolean = true

scala> (10, 1).nonEmpty
val res1: Boolean = true

scala> (1, 2) relate (3, 4)
val res2: rampart4s.Relation = Before

scala> ((1,2) relate (3,4)).invert
val res3: rampart4s.Relation = After

scala> (100 to 200) starts (100 to 300) // scala's Range
val res4: Boolean = true

scala> import rampart4s.Interval

scala> given Interval[Char, Char] = Interval.pointInstance[Char] // single value
lazy val given_Interval_Char_Char: rampart4s.Interval[Char, Char]

scala> "ABC".map('A' relate _)
val res5: IndexedSeq[rampart4s.Relation] = ArraySeq(Equal, Before, Before)

scala> import cats.data.NonEmptySet
scala> given Interval[NonEmptySet[Int], Int] with // custom comparison 
     |   def lesser(a: NonEmptySet[Int]): Int = a.reduceLeft(_ min _)
     |   def greater(a: NonEmptySet[Int]): Int = a.reduceLeft(_ max _)
     | 
// defined object given_Interval_NonEmptySet_Int
scala> NonEmptySet.of(3, 2, 6, 7) relate NonEmptySet.of(4, 5, 6) // same as (2, 7) relate (4, 6)
val res0: rampart4s.Relation = Contains
```