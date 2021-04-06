module Format : sig
  val rendered_length : (Format.formatter -> 'a -> unit) -> 'a -> int
  val green : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit

  val yellow
    :  (Format.formatter -> 'a -> unit)
    -> Format.formatter
    -> 'a
    -> unit

  val red : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
  val blue : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
  val faint : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
end

module Seq : sig
  val head_tail_exn : 'a Seq.t -> 'a * 'a Seq.t
end
