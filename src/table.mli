type alignment
type cell
type t

val left : alignment
val center : alignment
val right : alignment
val text : ?color:Printer.Color.t -> string -> cell
val of_list : columns:alignment list -> cell list list -> t
val pp : Format.formatter -> t -> unit
