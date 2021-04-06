type t

val make : max_size:int -> t Random.t
val make_seq : max_size:int -> t Seq.t Random.t
val of_list : max_size:int -> Int32.t list -> t
val of_seq : max_size:int -> Int32.t Seq.t -> t
val head_tail : t -> (Int32.t * t) option
val take : int -> t -> Int32.t list
val drop : int -> t -> t
val head : t -> Int32.t option
val map : (Int32.t -> Int32.t) -> t -> t
val max_size : t -> int
val set_max_size : int -> t -> t
