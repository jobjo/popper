type t

val make : Tag.t -> Int32.t list -> t

val data : t -> Int32.t list

val tag : t -> Tag.t
