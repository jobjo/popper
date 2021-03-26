module Seed : sig
  type t

  val make : int -> t
  val make_self_init : unit -> t
  val split : t -> t * t
end

type 'a t

val run : Seed.t -> 'a t -> 'a * Seed.t
val eval : Seed.t -> 'a t -> 'a
val make : (Seed.t -> 'a * Seed.t) -> 'a t
val return : 'a -> 'a t
val seed : Seed.t t
val int32 : Int32.t t
val range : int -> int -> int t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val map : ('a -> 'b) -> 'a t -> 'b t
val generate : init:'s -> ('s -> ('a * 's) t) -> 'a Seq.t t
val sequence : 'a t list -> 'a list t

module Syntax : sig
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end
