type t

val make : ?count:int -> ?verbose:unit -> (unit -> Proposition.t Sample.t) -> t
val suite : (string * t) list -> t
val run : ?seed:Random.Seed.t -> t -> unit
