type 'a t

val run : Input.t -> 'a t -> 'a Output.t

val tag : Tag.t -> 'a t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val return : 'a -> 'a t

val bind : 'a t -> ('a -> 'b t) -> 'b t

val both : 'a t -> 'b t -> ('a * 'b) t

val sequence : 'a t list -> 'a list t

val many : 'a t -> 'a list t

val int32 : int32 t

val range : int -> int -> int t

val one_of : 'a t list -> 'a t

val one_value_of : 'a list -> 'a t

val char : char t

val float : float t

val int64 : int64 t

val bool : bool t

val arrow : 'a t -> ('b -> 'a) t

val int : int t

val string : string t

module Syntax : sig
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end
