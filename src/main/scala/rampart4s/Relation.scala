package rampart4s
import cats.Order

enum Relation(private val inverted: String):
  case Before       extends Relation("After")
  case Meets        extends Relation("MetBy")
  case Overlaps     extends Relation("OverlappedBy")
  case FinishedBy   extends Relation("Finishes")
  case Contains     extends Relation("During")
  case Starts       extends Relation("StartedBy")
  case Equal        extends Relation("Equal")
  case StartedBy    extends Relation("Starts")
  case During       extends Relation("Contains")
  case Finishes     extends Relation("FinishedBy")
  case OverlappedBy extends Relation("Overlaps")
  case MetBy        extends Relation("Meets")
  case After        extends Relation("Before")
  def invert: Relation = Relation.valueOf(inverted)

object Relation:
  def apply[P, A](p: P, q: P)(using i: Interval[P, A])(using o: Order[A]): Relation =
    import cats.Comparison.{EqualTo as EQ, LessThan as LT, GreaterThan as GT}
    import i.{lesser, greater}
    val compare = (f: P => A, g: P => A) => o.comparison(f(p), g(q))
    //         |  p   |   q    |
    ( compare( lesser , lesser ),
      compare( lesser , greater),
      compare( greater, lesser ),
      compare( greater, greater)) match
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
