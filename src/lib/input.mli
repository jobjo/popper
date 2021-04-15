type t

val make : size:int -> t Random.t
val make_seq : size:int -> t Seq.t Random.t
val of_list : size:int -> Int32.t list -> t
val of_seq : size:int -> Int32.t Seq.t -> t
val head_tail : t -> (Int32.t * t) option
val take : int -> t -> Int32.t list
val drop : int -> t -> t
val head : t -> Int32.t option
val map : (Int32.t -> Int32.t) -> t -> t
val size : t -> int
val set_size : int -> t -> t
