val shrink :
  max_count:int ->
  Proposition.t Output.t ->
  Proposition.t Generator.t ->
  (int * (Format.formatter -> unit -> unit)) option Random.t
