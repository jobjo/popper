type t

type result =
  { num_shrinks : int
  ; num_attempts : int
  ; pp : Format.formatter -> unit -> unit
  ; output : Proposition.t Output.t
  }

val pp : Format.formatter -> t -> unit

val shrink
  :  max_size:int
  -> Consumed.t
  -> Proposition.t Generator.t
  -> result Random.t
