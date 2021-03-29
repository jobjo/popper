type t

(** [test ?count prop] creates a test *)
val test : ?count:int -> Proposition.t Generator.t -> t

(** [unit f] createsa a unit test *)
val unit : (unit -> Proposition.t) -> t

(** [suite tests] packs the given list of tests *)
val suite : (string * t) list -> t

(** [run ]*)
val run : ?seed:Random.Seed.t -> t -> unit
