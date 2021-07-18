type 'a t

val run : on_exception:(exn -> 'a) -> Input.t -> 'a t -> 'a Output.t
val map : ('a -> 'b) -> 'a t -> 'b t
val return : 'a -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val both : 'a t -> 'b t -> ('a * 'b) t
val size : int t
val sized : (int -> 'a t) -> 'a t
val resize : int -> 'a t -> 'a t
val one_of : 'a t list -> 'a t
val one_value_of : 'a list -> 'a t
val choose : (float * 'a t) list -> 'a t
val delayed : (unit -> 'a t) -> 'a t
val sequence : 'a t list -> 'a list t
val unit : unit t
val int : int t
val int32 : int32 t
val int64 : int64 t
val char : char t
val float : float t
val bool : bool t
val fn : 'a t -> ('b -> 'a) t
val string : string t
val bytes : bytes t
val option : 'a t -> 'a option t
val result : ok:'a t -> error:'b t -> ('a, 'b) result t
val list : 'a t -> 'a list t
val array : 'a t -> 'a array t
val log_key_value : string -> string -> unit t
val with_log : string -> (Format.formatter -> 'a -> unit) -> 'a t -> 'a t
val with_consumed : 'a t -> ('a * Consumed.t) t
val tag_name : string -> 'a t -> 'a t

module Syntax : sig
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

module Int : sig
  val range : int -> int -> int t
  val small : int t
  val medium : int t
  val positive : int t
  val negative : int t
end

module Float : sig
  val range : float -> float -> float t
  val small : float t
  val medium : float t
  val positive : float t
  val negative : float t
end

module List : sig
  val of_length : int -> 'a t -> 'a list t
  val range : int -> int -> 'a t -> 'a list t
  val non_empty : 'a t -> 'a list t
end

module Array : sig
  val of_length : int -> 'a t -> 'a array t
  val range : int -> int -> 'a t -> 'a array t
  val non_empty : 'a t -> 'a array t
end

module String : sig
  val of_length : int -> string t
  val range : int -> int -> string t
  val alpha_numeric : string t
  val numeric : string t
  val alpha : string t
  val upper : string t
  val lower : string t
end

module Bytes : sig
  val of_length : int -> bytes t
  val range : int -> int -> bytes t
  val alpha_numeric : bytes t
  val numeric : bytes t
  val alpha : bytes t
  val upper : bytes t
  val lower : bytes t
end

module Tuple : sig
  val pair : 'a t -> 'b t -> ('a * 'b) t
  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
end
