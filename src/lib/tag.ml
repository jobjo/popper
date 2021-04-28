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
  | List
  | Choice
  | Name of string
  | Sub_list

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
  | List -> Printf.sprintf "list"
  | Choice -> "choice"
  | Name s -> Printf.sprintf "name[%s]" s
  | Sub_list -> "sub-list"
