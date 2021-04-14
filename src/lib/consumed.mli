type t =
  | Value of Int32.t
  | Empty
  | Add of t * t
  | Tag of Tag.t * t

val empty : t
val is_empty : t -> bool
val add : t -> t -> t
val tag : Tag.t -> t -> t
val value : int32 -> t
val to_list : t -> ((Tag.t * int) list * int32) list
val pp : Format.formatter -> t -> unit
