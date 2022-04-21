package rampart4s3

enum Relation:
  case
    Dummy, // FIXME: remove this
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
