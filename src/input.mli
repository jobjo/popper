type t

val make : int -> t

val of_list : Int32.t list -> t

val make_self_init : unit -> t

val make_seq : int -> t Seq.t

val make_seq_self_init : unit -> t Seq.t

val head_tail : t -> (Int32.t * t) option

val take : int -> t -> Int32.t list

val drop : int -> t -> t

val head : t -> Int32.t option

val map : (Int32.t -> Int32.t) -> t -> t
