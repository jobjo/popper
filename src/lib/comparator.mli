type 'a t

val make : ('a -> 'a -> int) -> (Format.formatter -> 'a -> unit) -> 'a t
val compare : 'a t -> 'a -> 'a -> int
val pp : 'a t -> Format.formatter -> 'a -> unit
val int : int t
val int32 : int32 t
val int64 : int64 t
val float : float t
val bool : bool t
val char : char t
val string : string t
val bytes : bytes t
val unit : unit t
val tuple : 'a t -> 'b t -> ('a * 'b) t
val list : 'a t -> 'a list t
val array : 'a t -> 'a array t
val option : 'a t -> 'a option t
val result : ok:'a t -> error:'e t -> ('a, 'e) result t
