type 'a t

val make : ('a -> 'a -> int) -> (Format.formatter -> 'a -> unit) -> 'a t
val compare : 'a t -> 'a -> 'a -> int
val eq : 'a t -> 'a -> 'a -> bool
val pp : 'a t -> Format.formatter -> 'a -> unit
val tuple : 'a t -> 'b t -> ('a * 'b) t
val list : 'a t -> 'a list t
val int : int t
val float : float t
val bool : bool t
val string : string t
val option : 'a t -> 'a option t
