type 'a result =
  { num_attempts : int
  ; num_explored : int
  ; node : 'a
  }

module type Config = sig
  type t

  val max_tries : int
  val compare : t -> t -> int
  val keep : t -> t option
  val modify : t -> t Random.t
end

module type S = sig
  type t

  val search : t -> t result Random.t
end

module Make (C : Config) : S with type t = C.t
