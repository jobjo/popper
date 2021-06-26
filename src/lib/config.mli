type t

val default : t
val num_samples : int -> t
val max_shrinks : int -> t
val seed : int list -> t
val verbose : t
val max_input_length : int -> t
val max_num_discarded : int -> t
val max_size : int -> t
val formatter : Format.formatter -> t
val symbol_pass : string -> t
val symbol_fail : string -> t
val symbol_discard : string -> t
val get_num_samples : t -> int
val get_max_shrinks : t -> int
val get_max_shrink_modify_attempts : t -> int
val get_max_size : t -> int
val get_seed : t -> int list
val get_verbose : t -> bool
val get_max_input_length : t -> int
val get_max_num_discarded : t -> int
val get_formatter : t -> Format.formatter
val get_symbol_pass : t -> string
val get_symbol_fail : t -> string
val get_symbol_discard : t -> string
val all : t list -> t
