type t

val empty : t
val add : t -> t -> t
val pp : Format.formatter -> t -> unit
val of_pp : (Format.formatter -> unit) -> t
