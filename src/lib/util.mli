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

module List : sig
  val head_opt : 'a list -> 'a option
  val concat_map : ('a -> 'b list) -> 'a list -> 'b list
end

module Seq : sig
  val head_tail_exn : 'a Seq.t -> 'a * 'a Seq.t
  val take : int -> 'a Seq.t -> 'a Seq.t
  val drop : int -> 'a Seq.t -> 'a Seq.t
  val unfold : ('a -> ('b * 'a) option) -> 'a -> unit -> 'b Seq.node
end
