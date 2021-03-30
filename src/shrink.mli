val shrink
  :  max_count_find_next:int
  -> max_count_shrinks:int
  -> Proposition.t Output.t
  -> Proposition.t Generator.t
  -> (int * (Format.formatter -> unit -> unit) * Proposition.t Output.t) option
     Random.t
