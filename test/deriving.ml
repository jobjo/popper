type t1 =
  | A
  | B
[@@deriving show, eq, popper]

type t2 =
  | A of int
  | B of t2
[@@deriving show, eq, popper]

type t3 =
  [ `True
  | `False
  ]
[@@deriving show, eq, popper]

type t4 =
  [ `Text of string list
  | `Div of t4
  ]
[@@deriving show, eq, popper]

type t5 =
  { s : string
  ; i : int
  }
[@@deriving show, eq, popper]

type t6 =
  { x : string option list
  ; y : int
  ; z : (bool list, string) result list
  }
[@@deriving show, eq, popper]

type t7 =
  | A7 of
      { s : string
      ; i : int
      }
  | B7
[@@deriving show, eq, popper]

type t8 = int [@@deriving show, eq, popper]

type t9 =
  | Leaf of int
  | Node of t10

and t10 =
  { value : int
  ; left : t9
  ; right : t9
  }
[@@deriving show, eq, popper]

type 'a t11 = { value : 'a } [@@deriving eq, show, popper]
type t12 = int t11 [@@deriving eq, show, popper]
type t13 = T13 of int t11 [@@deriving eq, show, popper]
type t14 = { t14 : int t11 } [@@deriving eq, show, popper]

type ('a, 'b, 'c) t15 =
  | T15A of { a : 'a }
  | T15B of { b : 'b }
  | T15C of 'c
  | T15D of 'a * 'b * 'c
[@@deriving eq, show, popper]

type t16 = { t16 : (int, bool, string option) t15 }
[@@deriving eq, show, popper]

let make_test name comparator generator =
  let open Popper in
  let open Popper.Generator.Syntax in
  ( name
  , Test.test (fun () ->
      let* x = generator in
      Test.equal comparator x x) )

let suite =
  Popper.Test.suite
    [ make_test "t1" t1_comparator generate_t1
    ; make_test "t2" t2_comparator generate_t2
    ; make_test "t3" t3_comparator generate_t3
    ; make_test "t4" t4_comparator generate_t4
    ; make_test "t5" t5_comparator generate_t5
    ; make_test "t6" t6_comparator generate_t6
    ; make_test "t7" t7_comparator generate_t7
    ; make_test "t8" t8_comparator generate_t8
    ; make_test "t9" t9_comparator generate_t9
    ; make_test "t10" t10_comparator generate_t10
    ; make_test "t12" t12_comparator generate_t12
    ; make_test "t13" t13_comparator generate_t13
    ; make_test "t14" t14_comparator generate_t14
    ; make_test "t16" t16_comparator generate_t16
    ]
