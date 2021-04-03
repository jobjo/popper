type t =
  | Size
  | Sign
  | Function
  | Char
  | Int
  | Float
  | Bool
  | Value
  | Operator
[@@deriving show]

val is_operator : t -> bool
val is_value : t -> bool
val to_string : t -> string
