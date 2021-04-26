type t

val default : t
val num_samples : int -> t
val max_shrinks : int -> t
val seed : Random.Seed.t -> t
val verbose : t
val set : t list -> t
val get_num_samples : t -> int
val get_max_shrinks : t -> int
val get_max_shrink_modify_attempts : t -> int
val get_max_size : t -> int
val get_seed : t -> Random.Seed.t
val get_verbose : t -> bool
