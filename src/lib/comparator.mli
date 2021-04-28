type 'a t

val make : ('a -> 'a -> int) -> (Format.formatter -> 'a -> unit) -> 'a t
val compare : 'a t -> 'a -> 'a -> int
val pp : 'a t -> Format.formatter -> 'a -> unit
val int : int t
val float : float t
val bool : bool t
val string : string t
val tuple : 'a t -> 'b t -> ('a * 'b) t
val list : 'a t -> 'a list t
val array : 'a t -> 'a array t
val option : 'a t -> 'a option t
val result : ok:'a t -> error:'e t -> ('a, 'e) result t
