package rampart4s3

enum Relation:
  case
    Before,
    Meets,
    Overlaps,
    FinishedBy,
    Contains,
    Starts,
    Equal,
    StartedBy,
    During,
    Finishes,
    OverlappedBy,
    MetBy,
    After

object Relation:
  extension (rel: Relation)
    def invert: Relation = rel match
      case Before       => After 
      case Meets        => MetBy 
      case Overlaps     => OverlappedBy 
      case FinishedBy   => Finishes 
      case Contains     => During 
      case Starts       => StartedBy 
      case Equal        => Equal 
      case StartedBy    => Starts 
      case During       => Contains 
      case Finishes     => FinishedBy 
      case OverlappedBy => Overlaps 
      case MetBy        => Meets 
      case After        => Before 
