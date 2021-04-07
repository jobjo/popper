type ('a, 'b) value =
  { a : 'a
  ; b : 'b
  }
[@@deriving eq, show, popper]
