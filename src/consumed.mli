type t

val empty : t
val add : t -> t -> t
val tag : Tag.t -> t -> t
val make : Tag.t -> int32 list -> t
val to_list : t -> (Tag.t * int32) list
