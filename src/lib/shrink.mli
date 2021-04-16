type t

type result =
  { num_shrinks : int
  ; num_attempts : int
  ; pp : Format.formatter -> unit -> unit
  ; output : Proposition.t Output.t
  }

val pp : Format.formatter -> t -> unit

val shrink
  :  Proposition.t Output.t
  -> Proposition.t Generator.t
  -> result Random.t
