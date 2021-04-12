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

let is_operator t = t = Operator || t = Size
let is_value t = not @@ is_operator t

let to_string = function
  | Sign -> "sign"
  | Function -> "function"
  | Char -> "char"
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Value -> "value"
  | Operator -> "operator"
  | Size -> "size"
