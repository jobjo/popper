module Format : sig
  val rendered_length : (Format.formatter -> 'a -> unit) -> 'a -> int
end

module Seq : sig
  val head_tail_exn : 'a Seq.t -> 'a * 'a Seq.t
end
