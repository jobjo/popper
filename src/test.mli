type t

val test : ?count:int -> Proposition.t Generator.t -> t
val unit : (unit -> Proposition.t) -> t
val suite : (string * t) list -> t
val run : ?seed:Random.Seed.t -> t -> unit

val equal
  :  ?loc:string
  -> 'a Comparable.t
  -> 'a
  -> 'a
  -> Proposition.t Generator.t

val is_true : ?loc:string -> bool -> Proposition.t Generator.t
