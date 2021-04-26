type config =
  { max_shrinks : int
  ; max_shrink_rounds : int
  ; max_shrink_modify_attempts : int
  ; num_samples : int
  ; max_size : int
  ; seed : Random.Seed.t
  ; verbose : bool
  }

type t = config -> config

let default =
  { max_shrinks = 100
  ; max_shrink_rounds = 10
  ; max_shrink_modify_attempts = 100
  ; num_samples = 300
  ; max_size = 100
  ; seed = Random.Seed.make 42
  ; verbose = false
  }

let num_samples num_samples c = { c with num_samples }
let max_shrinks max_shrinks c = { c with max_shrinks }
let seed seed c = { c with seed }
let verbose c = { c with verbose = true }
let set cs c = List.fold_left (fun c f -> f c) c cs
let get_num_samples f = (f default).num_samples
let get_max_shrinks f = (f default).max_shrinks
let get_max_shrink_modify_attempts f = (f default).max_shrink_modify_attempts
let get_max_size f = (f default).max_size
let get_seed f = (f default).seed
let get_verbose f = (f default).verbose
let default = Fun.id
