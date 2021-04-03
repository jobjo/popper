module Format : sig
  val rendered_length : (Format.formatter -> 'a -> unit) -> 'a -> int
end
