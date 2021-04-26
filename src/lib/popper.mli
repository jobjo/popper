module Comparator : sig
  include module type of Comparator (** @inline*)
end

module Sample : sig
  include module type of Sample (** @inline*)
end

module Proposition : sig
  include module type of Proposition (** @inline*)
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

module Test : sig
  type t
end

module Config : sig
  include module type of Config (** @inline *)
end

exception Popper_error

(** [test ?count f] creates a test that when run evaluates the given function on a number of arbitrary inputs. *)
val test : ?config:Config.t -> (unit -> Proposition.t Sample.t) -> Test.t

val suite : (string * Test.t) list -> Test.t
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
val run : ?config:Config.t -> Test.t -> unit
val run_test : (unit -> Proposition.t Sample.t) -> unit

val with_log
  :  string
  -> (Format.formatter -> 'a -> unit)
  -> 'a Sample.t
  -> 'a Sample.t
