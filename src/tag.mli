type t =
  | Sign
  | Function
  | Char
  | Int
  | Float
  | Bool
  | Value
  | Operator

val is_operator : t -> bool
val is_value : t -> bool
