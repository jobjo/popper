type t = private
  | Pass
  | Fail of
      { pp : Format.formatter -> unit -> unit
      ; location : string option
      }
  | Discard

val pass : t
val fail : ?loc:string -> (Format.formatter -> unit -> unit) -> t
val fail_with : ?loc:string -> string -> t
val discard : t
val equal : ?loc:string -> 'a Comparator.t -> 'a -> 'a -> t
val less_than : ?loc:string -> 'a Comparator.t -> 'a -> 'a -> t
val greater_than : ?loc:string -> 'a Comparator.t -> 'a -> 'a -> t
val less_equal_than : ?loc:string -> 'a Comparator.t -> 'a -> 'a -> t
val greater_equal_than : ?loc:string -> 'a Comparator.t -> 'a -> 'a -> t
val is_true : ?loc:string -> bool -> t
val is_false : ?loc:string -> bool -> t
val all : t list -> t
val any : t list -> t
