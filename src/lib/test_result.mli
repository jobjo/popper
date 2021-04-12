type failure =
  { num_shrinks : int
  ; num_attempts : int
  ; pp : Format.formatter -> unit -> unit
  ; explanation : string
  ; location : string option
  }

type discard = { num_discarded : int }

type status =
  | Pass
  | Fail of failure
  | Discarded of discard

type result =
  { name : string option
  ; num_passed : int
  ; status : status
  ; time : float
  ; log : Log.t
  ; is_unit : bool
  }

type t =
  { num_passed : int
  ; num_failed : int
  ; num_discarded : int
  ; time : float
  ; results : result list
  }

val pp : Format.formatter -> t -> unit
