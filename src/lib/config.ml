type config =
  { max_shrinks : int
  ; max_shrink_rounds : int
  ; max_shrink_modify_attempts : int
  ; num_samples : int
  ; max_size : int
  ; seed : int list
  ; verbose : bool
  ; max_input_length : int
  ; max_num_discarded : int
  ; formatter : Format.formatter
  ; symbol_pass : string
  ; symbol_fail : string
  ; symbol_discard : string
  }

type t = config -> config

let default =
  { max_shrinks = 100
  ; max_shrink_rounds = 10
  ; max_shrink_modify_attempts = 100
  ; num_samples = 300
  ; max_size = 100
  ; seed = [ 1; 23; 45 ]
  ; verbose = false
  ; max_input_length = 10_000
  ; max_num_discarded = 1000
  ; formatter = Format.std_formatter
  ; symbol_pass = "✓"
  ; symbol_fail = "✖"
  ; symbol_discard = "☐"
  }

let num_samples num_samples c = { c with num_samples }
let max_shrinks max_shrinks c = { c with max_shrinks }
let seed seed c = { c with seed }
let verbose c = { c with verbose = true }
let max_size max_size c = { c with max_size }
let max_input_length max_input_length c = { c with max_input_length }
let max_num_discarded max_num_discarded c = { c with max_num_discarded }
let formatter formatter c = { c with formatter }
let symbol_pass symbol_pass c = { c with symbol_pass }
let symbol_fail symbol_fail c = { c with symbol_fail }
let symbol_discard symbol_discard c = { c with symbol_discard }
let all cs c = List.fold_left (fun c f -> f c) c cs
let get_num_samples f = (f default).num_samples
let get_max_shrinks f = (f default).max_shrinks
let get_max_shrink_modify_attempts f = (f default).max_shrink_modify_attempts
let get_max_size f = (f default).max_size
let get_seed f = (f default).seed
let get_verbose f = (f default).verbose
let get_max_input_length f = (f default).max_input_length
let get_max_num_discarded f = (f default).max_num_discarded
let get_formatter f = (f default).formatter
let get_symbol_pass f = (f default).symbol_pass
let get_symbol_fail f = (f default).symbol_fail
let get_symbol_discard f = (f default).symbol_discard
let default = Fun.id
