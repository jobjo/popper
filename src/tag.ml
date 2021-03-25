type t = Sign | Function | Char | Int | Float | Bool | Value | Operator

let is_operator t = t = Operator

let is_value t = t <> Operator
