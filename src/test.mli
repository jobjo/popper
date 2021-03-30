type t

val test : ?count:int -> Proposition.t Generator.t -> t
val unit : (unit -> Proposition.t) -> t
val suite : (string * t) list -> t
val run : ?seed:Random.Seed.t -> t -> unit

val equals
  :  (Format.formatter -> 'a -> unit)
  -> 'a
  -> 'a
  -> Proposition.t Generator.t

val is_true : bool -> Proposition.t Generator.t
