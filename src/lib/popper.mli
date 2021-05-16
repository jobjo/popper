(** Popper is an OCaml testing library that can be used for writing simple
    unit-tests as well as property-based ones. Its underlying design is inspired
    by the Python library Hypothesis. *)

(** {1 Modules } *)

module Comparator : sig
  (** {1 Types } *)

  (** This module exposes a type ['a t]. A value of type [a t] is used for
      comparing and pretty printing values of type [a].*)

  type 'a t

  (** {2 Basic operators } *)

  (** [make compare pp] constructs a comparator using the given [compare] to
      compare value and [pp] to print them. *)
  val make : ('a -> 'a -> int) -> (Format.formatter -> 'a -> unit) -> 'a t

  (** [compare c x y] compares [x] and [y] using the [c] comparator. *)
  val compare : 'a t -> 'a -> 'a -> int

  (** [pp c x] prints [x] using the [c] comparator. *)
  val pp : 'a t -> Format.formatter -> 'a -> unit

  (** {3 Primitive comparators } *)

  (** [int] is an integer comparator.*)
  val int : int t

  (** [float] is a float comparator. *)
  val float : float t

  (** [bool] is a bool comparator. *)
  val bool : bool t

  (** [string] is a string comparator. *)
  val string : string t

  (** {4 Combinators } *)

  (** [tuple c1 c2] creates a comparator for tuples using the comparators [c1]
      and [c2]. *)
  val tuple : 'a t -> 'b t -> ('a * 'b) t

  (** [tuple c] creates a comparator for lists using the the comparator [c] for
      comparing elements. *)
  val list : 'a t -> 'a list t

  (** [array c] creates a comparator for arrays using the the comparator [c] for
      comparing elements. *)
  val array : 'a t -> 'a array t

  (** [option c] creates a comparator for options using the the comparator [c]
      for comparing elements. *)
  val option : 'a t -> 'a option t

  (** [option ~ok ~error] creates a comparator for result values using the the
      comparators [ok] and [error]. *)
  val result : ok:'a t -> error:'e t -> ('a, 'e) result t
end

module Consumed : sig
  (** {1 Types }*)

  type t

  (** {1 Functions} *)

  (** [pp out t] pretty prints the given [t] using the formatter [out]. *)
  val pp : Stdlib.Format.formatter -> t -> unit
end

module Sample : sig
  (** {1 Types }*)

  (** A value of type ['a t] is a parser that when run with an [Input.t] value,
      produces some output containing a value of type ['a]. *)
  type 'a t

  (** {1 Basic Combinators } *)

  (** [map f s] creates a sample that when run maps its output value using the
      function [f]. *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** [return x] is a sample that does not read any input and always produces a
      value containing [x]. *)
  val return : 'a -> 'a t

  (** [bind s f] is a sample that when run, first executes [s] on the given
      input, then applies its output value to [f] in order to produe a new
      sample that is run on the remaining input. This is the monadic operator
      for sequencing sample operations. *)
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  (** [both s1 s2] is a sample that when run executes [s1] followed by [s2] and
      combines their results. *)
  val both : 'a t -> 'b t -> ('a * 'b) t

  (** [size] returns the current [max-size] argument. *)
  val size : int t

  (** [sized f] lifts the function [f] that depend on a size argument to a
      sample. *)
  val sized : (int -> 'a t) -> 'a t

  (** [resize n s] creates a sample that when run fixes the size parameter to
      [n].*)
  val resize : int -> 'a t -> 'a t

  (** [one_of ss] is a sample that selects one of the given sample from the list
      [ss].*)
  val one_of : 'a t list -> 'a t

  (** [one_vlaue_of xs] same as [one_of] but lifts all values to samples using
      [return]. *)
  val one_value_of : 'a list -> 'a t

  (** [choose fss] chooses one sample from the list with a probability
      corresponding to the relative weight, as specified by the weight and
      sample pairs in [fss]. *)
  val choose : (float * 'a t) list -> 'a t

  (** [delayed f] is sample that delays the production of the sample using [f]
      until run *)
  val delayed : (unit -> 'a t) -> 'a t

  (** [sequence ss] is a sample that runs each sample in [ss] and combines their
      results in a list. *)
  val sequence : 'a t list -> 'a list t

  (** {1 Primitives } *)

  (** [unit] is a sample that produce a unit value without consuming any input. *)
  val unit : unit t

  (** [int] is a sample that produces [int] values. *)
  val int : int t

  (** [int32] is a sample that produces [int32] values. *)
  val int32 : int32 t

  (** [int64] is a sample that produces [int64] values. *)
  val int64 : int64 t

  (** [char] is a sample that produces [char] values. *)
  val char : char t

  (** [float] is a sample that produces [float] values. *)
  val float : float t

  (** [bool] is a sample that produces [bool] values. *)
  val bool : bool t

  (** [fn s] is a sample that produces a function value. *)
  val fn : 'a t -> ('b -> 'a) t

  (** [char] is a sample that produces [char] values. *)
  val string : string t

  (** {1 Combinators for higher-kinded types } *)

  (** [option s] is a sample that either produces a value [None] or a value
      [Some] using [s]. *)
  val option : 'a t -> 'a option t

  (** [result ~ok ~error] is a sample that produces a result value, either using
      the [ok] sample, or an error using the [error] sample. *)
  val result : ok:'a t -> error:'b t -> ('a, 'b) result t

  (** [list s] is a sample that produces a list using the give [s]. The size of
      the list is not determined but is influenced by the implicit [size]
      parameter. *)
  val list : 'a t -> 'a list t

  (** {1 Debugging combinators } *)

  (** [log_key_value key value] is a sample that when run logs the given [key]
      and [value] pair without consuming any input. *)
  val log_key_value : string -> string -> unit t

  (** [with_log key pp s] enhances the given sample [s] so as to also log its
      result using the [key] and printer [pp] for producing a string value. *)
  val with_log : string -> (Format.formatter -> 'a -> unit) -> 'a t -> 'a t

  (** [with_consumed s] is a sample produces a pair of a value using [s] and its
      consumed data. This would only be used for debugging purposes. *)
  val with_consumed : 'a t -> ('a * Consumed.t) t

  (** [tag_name name s] is a sample that when run, applies [s] but tags its
      consumed input with the given [name]. Mostly useful for debuggin. *)
  val tag_name : string -> 'a t -> 'a t

  module Syntax : sig
    (** [let* x = s in body] is the same as [bind s (fun x -> body)]. *)
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    (** [let+ x = s in body] is the same as [map (fun x -> body) s]. *)
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    (** [let* x = s1 and* y = s2 in body] is the same as [bind s1 (fun x -> bind
        s2 (fun y -> body)] *)
    val ( and* ) : 'a t -> 'b t -> ('a * 'b) t

    (** [let+ x = s1 and+ y = s2 in body] is the same as [map (fun (x,y) ->
        body) (both s1 s2)]. *)
    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  end

  module Int : sig
    (** [range min max] is a sample that produces a value [x] such that [min <=
        x < max]. *)
    val range : int -> int -> int t

    (** [small] is a sample producing small integers in the rante [-10 to 10]. *)
    val small : int t

    (** [medium] is a sample that produces value in the range [-1000 to 1000]. *)
    val medium : int t

    (** [positive] is a sample that produces positive values. *)
    val positive : int t

    (** [negative] is a sample that produces negative values. *)
    val negative : int t
  end

  module Float : sig
    (** [range min max] is a sample that produces a value [x] such that [min <=
        x < max]. *)
    val range : float -> float -> float t

    (** [small] is a sample producing small integers in the rante [-10 to 10]. *)
    val small : float t

    (** [medium] is a sample that produces value in the range [-1000 to 1000]. *)
    val medium : float t

    (** [positive] is a sample that produces positive values. *)
    val positive : float t

    (** [negative] is a sample that produces negative values. *)
    val negative : float t
  end

  module List : sig
    (** [of_length n] is a sample that always produces a list of fixed size [n]. *)
    val of_length : int -> 'a t -> 'a list t

    (** [range min max s] is a sample that always produces a list of size [n],
        where [min <= n < max], using the given sample [s]. *)
    val range : int -> int -> 'a t -> 'a list t

    (** [non_empty s] produces a non-empty list using the sample [s] to draw the
        elements. *)
    val non_empty : 'a t -> 'a list t
  end

  module Array : sig
    (** [of_length n] is a sample that always produces an array of fixed size
        [n]. *)
    val of_length : int -> 'a t -> 'a array t

    (** [range min max s] is a sample that always produces an array of size [n],
        where [min <= n < max], using the given sample [s]. *)
    val range : int -> int -> 'a t -> 'a array t

    (** [non_empty s] produces a non-empty array using the sample [s] to draw
        the elements. *)
    val non_empty : 'a t -> 'a array t
  end

  module String : sig
    (** [of_length n] is a sample that always produces a string of fixed size
        [n]. *)
    val of_length : int -> string t

    (** [range min max s] is a sample that always produces a string of size [n],
        where [min <= n < max], using the given sample [s]. *)
    val range : int -> int -> string t

    (** [alpha_numeric] is a sample that produces strings containing
        alhpa-numeric characthers. *)
    val alpha_numeric : string t

    (** [numeric] is a sample that produces strings containing numbers *)
    val numeric : string t

    (** [alpha] a sample that produces strings containing characters a-z, both
        upper and lower case. *)
    val alpha : string t

    (** [upper] is sample that always produces upper-case characters. *)
    val upper : string t

    (** [upper] is sample that always produces lower-case characters. *)
    val lower : string t
  end

  module Tuple : sig
    (** [pair s1 s2] is a sample that produces a pair using [s1] and [s2] and
        combines their results. *)
    val pair : 'a t -> 'b t -> ('a * 'b) t

    (** [tripple s1 s2 s3] is a sample that produces a tuple using [s1], [s2]
        and [s3] and combines their results. *)
    val tripple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

    (** [quad s1 s2 s3 s4] is a sample that produces a tuple using [s1], [s2] ,
        [s3] and [s4] and combines their results. *)
    val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  end
end

module Proposition : sig
  (** {1 Types } *)

  type t

  (** {1 Primitives } *)

  (** [pass] is a proposition that true *)
  val pass : t

  (** [fail ?loc pp] is a failing proposition with location [loc] if given and
      printer [pp] for the error message *)
  val fail : ?loc:string -> (Format.formatter -> unit -> unit) -> t

  (** [fail_with ?loc msg] is a proposition that fails with location [loc] if
      given and message [msg] *)
  val fail_with : ?loc:string -> string -> t

  (** [discard] is a proposition that is discarded. *)
  val discard : t

  (** [equal ?loc cmp x y] returns a proposition [pass] only if [x] and [y] are
      equal using the given comparator [cmp]. If [loc] is passed, it reports the
      location string in case of failure. *)
  val equal : ?loc:string -> 'a Comparator.t -> 'a -> 'a -> t

  (** [less_than ?loc cmp x y] returns a proposition [pass] only if [x] is less
      than [y] when using the given compartor [cmp]. If [loc] is passed, it
      reports the location string in case of failure. *)
  val less_than : ?loc:string -> 'a Comparator.t -> 'a -> 'a -> t

  (** [greater_than ?loc cmp x y] returns a proposition [pass] only if [x] is
      greater than [y] when using the given compartor [cmp]. If [loc] is passed,
      it reports the location string in case of failure. *)
  val greater_than : ?loc:string -> 'a Comparator.t -> 'a -> 'a -> t

  (** [less_equal_than ?loc cmp x y] returns a proposition [pass] only if [x] is
      less than or equal to [y] when using the given compartor [cmp]. If [loc]
      is passed, it reports the location string in case of failure. *)
  val less_equal_than : ?loc:string -> 'a Comparator.t -> 'a -> 'a -> t

  (** [greater_equal_than ?loc cmp x y] returns a proposition [pass] only if [x]
      is greater than or equal to [y] when using the given compartor [cmp]. If
      [loc] is passed, it reports the location string in case of failure. *)
  val greater_equal_than : ?loc:string -> 'a Comparator.t -> 'a -> 'a -> t

  (** [is_true ?loc b] returns a proposition [pass] if [b] is [true]. Otherwise
      [fail] with location [loc] if given. *)
  val is_true : ?loc:string -> bool -> t

  (** [is_false ?loc b] returns a proposition [pass] if [b] is [false].
      Otherwise [fail] with location [loc] if given. *)
  val is_false : ?loc:string -> bool -> t

  (** {1 Combinators } *)

  (** [all ps] is a proposition that only passes if all given propositions in
      [ps] are [pass]. *)
  val all : t list -> t

  (** [all ps] is a proposition that passes if any of the given propositions in
      [ps] are [pass]. *)
  val any : t list -> t
end

module Test : sig
  type t
end

module Config : sig
  (** {1 Types }*)

  type t

  (** {1 Options }*)

  (** [num_samples n] is a configuration that specifies the number of samples to
      explore for property based tests. *)
  val num_samples : int -> t

  (** [seed s] is a configuration that sets the seed to be used when running
      tests. *)
  val seed : int list -> t

  (** [verbose] is a configuration that turns on verbose mode. That means that
      all logged values will be displayed for each test. *)
  val verbose : t

  (** [max_input_length n] is a configuration that specifies the maximum length
      of consumed input. Once a sample is run that exceed that length, only
      zeros are produced. *)
  val max_input_length : int -> t

  (** [max_size n] sets the maximum size parameter to be used when running
      tests. The default value is 100. With a large size paratmer, larger values
      are typically sampled. *)
  val max_size : int -> t

  (** {2 Combinators }*)

  (** [all cs] combines all configuration options [cs]. In case there are
      overlaps, the values at the end of the list take precedence. *)
  val all : t list -> t
end

(** {1 Types and exceptions } *)

exception Test_failure

(** {1 Constructing tests } *)

(** [test ?config f] creates a test that when run evaluates [f] that produces a
    [Proposition.t Sample.t] value. If the the sample consumes any input, it is
    run a number of times until it either finds some input value that causes the
    sample to yield a failing proposition, or until it passes specified number
    of runs (default is 300). If [config] is given it takes all the specified
    overrides into account.*)
val test : ?config:Config.t -> (unit -> Proposition.t Sample.t) -> Test.t

(** [suite ts] packs the list of name and test pairs, [ts], into a single test
    suite. *)
val suite : (string * Test.t) list -> Test.t

(** {1 Propositions } *)

(** [pass] is a sample that always returns the value [pass]. *)
val pass : Proposition.t Sample.t

(** [fail ?loc msg] is a sample that returns the value [fail] with location
    [loc] if given and error message [msg]. *)
val fail : ?loc:string -> string -> Proposition.t Sample.t

(** [equal ?loc cmp x y] is a sample that returns a proposition that is [pass]
    only if [x] and [y] are equal using the given comparator [cmp]. If [loc] is
    passed, it reports the location string in case of failure. *)
val equal : ?loc:string -> 'a Comparator.t -> 'a -> 'a -> Proposition.t Sample.t

(** [less_than ?loc cmp x y] is a sample that returns a proposition that is
    [pass] only if [x] is less than [y], using the given comparator [cmp]. If
    [loc] is passed, it reports the location string in case of failure. *)
val less_than
  :  ?loc:string
  -> 'a Comparator.t
  -> 'a
  -> 'a
  -> Proposition.t Sample.t

(** [greater_than ?loc cmp x y] is a sample that returns a proposition that is
    [pass] only if [x] is greater than [y], using the given comparator [cmp]. If
    [loc] is passed, it reports the location string in case of failure. *)
val greater_than
  :  ?loc:string
  -> 'a Comparator.t
  -> 'a
  -> 'a
  -> Proposition.t Sample.t

(** [greater_equal_than ?loc cmp x y] is a sample that returns a proposition
    that is [pass] only if [x] is greater than or equal to [y], using the given
    comparator [cmp]. If [loc] is passed, it reports the location string in case
    of failure. *)
val greater_equal_than
  :  ?loc:string
  -> 'a Comparator.t
  -> 'a
  -> 'a
  -> Proposition.t Sample.t

(** [less_than_equal ?loc cmp x y] is a sample that returns a proposition that
    is [pass] only if [x] is less than or equal to [y], using the given
    comparator [cmp]. If [loc] is passed, it reports the location string in case
    of failure. *)
val less_equal_than
  :  ?loc:string
  -> 'a Comparator.t
  -> 'a
  -> 'a
  -> Proposition.t Sample.t

(** [is_true ?loc b] is a sample that returns a proposition that is [pass] only
    if [b] is true. If [loc] is passed, it reports the location string in case
    of failure. *)
val is_true : ?loc:string -> bool -> Proposition.t Sample.t

(** [is_false ?loc b] is a sample that returns a proposition that is [pass] only
    if [b] is false. If [loc] is passed, it reports the location string in case
    of failure. *)
val is_false : ?loc:string -> bool -> Proposition.t Sample.t

(** [all ps] combines a list of proposition samples into a sample that only
    returns pass in case all returned propositions pass. *)
val all : Proposition.t Sample.t list -> Proposition.t Sample.t

(** [any ps] combines a list of proposition samples into a single sample that
    returns pass in case any of the returned propositions pass. *)
val any : Proposition.t Sample.t list -> Proposition.t Sample.t

(** {1 Running tests } *)

(** [check ?config f] runs a single anonymous test using the [config] settings
    if given. In case the test fails, an exception of type [Test_failure] is
    raised. *)
val check : ?config:Config.t -> (unit -> Proposition.t Sample.t) -> unit

(** [run ?config t] runs the given test [t] using the [config] settings if
    given. In case the test fails, an exception of type [Test_failure] is
    raised. *)
val run : ?config:Config.t -> Test.t -> unit
