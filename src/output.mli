type 'a t

val make : value:'a -> consumed:Consumed.t list -> remaining:Input.t -> 'a t

val set_consumed : Consumed.t list -> 'a t -> 'a t

val set_remaining : Input.t -> 'a t -> 'a t

val set_value : 'a -> 'a t -> 'a t

val value : 'a t -> 'a

val consumed : 'a t -> Consumed.t list

val remaining : 'a t -> Input.t

val tag : Tag.t -> 'a t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t
