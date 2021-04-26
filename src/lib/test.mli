type t

val make : ?config:Config.t -> (unit -> Proposition.t Sample.t) -> t
val suite : (string * t) list -> t
val run : ?config:Config.t -> t -> bool
