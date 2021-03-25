type seed

type 'a t

val make_seed : int -> seed

val make_seed_self_init : unit -> seed

val split_seed : seed -> seed * seed

val run : seed -> 'a t -> 'a * seed

val eval : seed -> 'a t -> 'a

val run_self_init : 'a t -> 'a

val make : (seed -> 'a * seed) -> 'a t

val return : 'a -> 'a t

val seed : seed t

val int32 : Int32.t t

val range : int -> int -> int t

val bind : 'a t -> ('a -> 'b t) -> 'b t

val map : ('a -> 'b) -> 'a t -> 'b t

val generate : ('a -> 'a t) -> 'a -> 'a Seq.t t

module Syntax : sig
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end
