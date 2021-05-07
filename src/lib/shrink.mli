type t

type result =
  { num_shrinks : int
  ; num_attempts : int
  ; pp : Format.formatter -> unit -> unit
  ; output : Proposition.t Output.t
  }

val pp : Format.formatter -> t -> unit

val shrink
  :  max_tries:int
  -> max_tries_modify:int
  -> num_shrink_rounds:int
  -> size:int
  -> Proposition.t Output.t
  -> Proposition.t Sample.t
  -> result Random.t
