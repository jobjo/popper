module Comparator : sig
  include module type of Comparator (** @inline*)
end

module Sample : sig
  include module type of Sample (** @inline*)
end

module Proposition : sig
  include module type of Proposition (** @inline*)
end

module Test : sig
  include module type of Test (** @inline*)
end

module Random : sig
  include module type of Random (** @inline*)
end

module Input : sig
  include module type of Input (** @inline*)
end

module Consumed : sig
  include module type of Consumed (** @inline*)
end

module Output : sig
  include module type of Output (** @inline*)
end

module Tag : sig
  include module type of Tag (** @inline*)
end

module Syntax : sig
  include module type of Sample.Syntax (** @inline*)
end

(** [test ?count f] creates a test that when run evaluates the given function on a number of arbitrary inputs. *)
val test
  :  ?count:int
  -> ?verbose:unit
  -> (unit -> Proposition.t Sample.t)
  -> Test.t

val suite : (string * Test.t) list -> Test.t
val run : ?seed:Random.Seed.t -> Test.t -> unit
val eq : ?loc:string -> 'a Comparator.t -> 'a -> 'a -> Proposition.t Sample.t
val lt : ?loc:string -> 'a Comparator.t -> 'a -> 'a -> Proposition.t Sample.t
val gt : ?loc:string -> 'a Comparator.t -> 'a -> 'a -> Proposition.t Sample.t
val gte : ?loc:string -> 'a Comparator.t -> 'a -> 'a -> Proposition.t Sample.t
val lte : ?loc:string -> 'a Comparator.t -> 'a -> 'a -> Proposition.t Sample.t
val is_true : ?loc:string -> bool -> Proposition.t Sample.t
val is_false : ?loc:string -> bool -> Proposition.t Sample.t
val all : Proposition.t Sample.t list -> Proposition.t Sample.t
val any : Proposition.t Sample.t list -> Proposition.t Sample.t
val pass : Proposition.t Sample.t
val fail : ?loc:string -> string -> Proposition.t Sample.t
val run_test : (unit -> Proposition.t Sample.t) -> unit

val with_log
  :  string
  -> (Format.formatter -> 'a -> unit)
  -> 'a Sample.t
  -> 'a Sample.t
