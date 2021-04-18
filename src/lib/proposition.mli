type t = private
  | Pass
  | Fail of
      { pp : Format.formatter -> unit -> unit
      ; location : string option
      }
  | Discard

val pass : t
val fail : ?loc:string -> (Format.formatter -> unit -> unit) -> t
val discard : t
val fail_with : ?loc:string -> string -> t
val eq : ?loc:string -> 'a Comparator.t -> 'a -> 'a -> t
val lt : ?loc:string -> 'a Comparator.t -> 'a -> 'a -> t
val gt : ?loc:string -> 'a Comparator.t -> 'a -> 'a -> t
val lte : ?loc:string -> 'a Comparator.t -> 'a -> 'a -> t
val gte : ?loc:string -> 'a Comparator.t -> 'a -> 'a -> t
val is_true : ?loc:string -> bool -> t
val is_false : ?loc:string -> bool -> t
val all : t list -> t
val any : t list -> t
