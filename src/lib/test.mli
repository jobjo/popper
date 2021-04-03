type t

val test : ?count:int -> (unit -> Proposition.t Generator.t) -> t
val suite : (string * t) list -> t
val run : ?seed:Random.Seed.t -> t -> unit

val equal
  :  ?loc:string
  -> 'a Comparator.t
  -> 'a
  -> 'a
  -> Proposition.t Generator.t

val is_true : ?loc:string -> bool -> Proposition.t Generator.t
