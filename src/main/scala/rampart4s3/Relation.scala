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
