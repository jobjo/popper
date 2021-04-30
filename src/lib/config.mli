type t

val default : t
val num_samples : int -> t
val max_shrinks : int -> t
val seed : int list -> t
val verbose : t
val max_input_length : int -> t
val max_size : int -> t
val get_num_samples : t -> int
val get_max_shrinks : t -> int
val get_max_shrink_modify_attempts : t -> int
val get_max_size : t -> int
val get_seed : t -> int list
val get_verbose : t -> bool
val get_max_input_length : t -> int
val all : t list -> t
