type ('a, 'b) value =
  { a : 'a
  ; b : 'b
  }
[@@deriving eq, show, popper]

type foo = (int, string) value [@@deriving eq, show, popper]
