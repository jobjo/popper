type t1 =
  | A
  | B
[@@deriving show, ord, popper]

type t2 =
  | A of int
  | B of t2
[@@deriving show, ord, popper]

type t3 =
  [ `True
  | `False
  ]
[@@deriving show, ord, popper]

type t4 =
  [ `Text of string list
  | `Div of t4
  ]
[@@deriving show, ord, popper]

type t5 =
  { s : string
  ; i : int
  }
[@@deriving show, ord, popper]

type t6 =
  { x : string option list
  ; y : int
  ; z : (bool list, string) result list
  }
[@@deriving show, ord, popper]

type t7 =
  | A7 of
      { s : string
      ; i : int
      }
  | B7
[@@deriving show, ord, popper]

type t8 = int [@@deriving show, ord, popper]

type t9 =
  | Leaf of int
  | Node of t10

and t10 =
  { value : int
  ; left : t9
  ; right : t9
  }
[@@deriving show, ord, popper]

type 'a t11 = { value : 'a } [@@deriving ord, show, popper]
type t12 = int t11 [@@deriving ord, show, popper]
type t13 = T13 of int t11 [@@deriving ord, show, popper]
type t14 = { t14 : int t11 } [@@deriving ord, show, popper]

type ('a, 'b, 'c) t15 =
  | T15A of { a : 'a }
  | T15B of { b : 'b }
  | T15C of 'c
  | T15D of 'a * 'b * 'c
[@@deriving ord, show, popper]

type t16 = { t16 : (int, bool, string option) t15 }
[@@deriving ord, show, popper]

type 'a t17 =
  | Node of 'a t17 * 'a t17
  | Leaf
  | T18 of { t18 : t18 option }
[@@deriving show, ord, popper]

and t18 = int t17 [@@deriving show, ord, popper]

let make_test name comparator sample =
  let open Popper in
  let open Syntax in
  ( name
  , test (fun () ->
      let* x = sample in
      eq comparator x x) )

let suite =
  Popper.Test.suite
    [ make_test "T1" t1_comparator sample_t1
    ; make_test "T2" t2_comparator sample_t2
    ; make_test "T3" t3_comparator sample_t3
    ; make_test "T4" t4_comparator sample_t4
    ; make_test "T5" t5_comparator sample_t5
    ; make_test "T6" t6_comparator sample_t6
    ; make_test "T7" t7_comparator sample_t7
    ; make_test "T8" t8_comparator sample_t8
    ; make_test "T9" t9_comparator sample_t9
    ; make_test "T10" t10_comparator sample_t10
    ; make_test "T12" t12_comparator sample_t12
    ; make_test "T13" t13_comparator sample_t13
    ; make_test "T14" t14_comparator sample_t14
    ; make_test "T16" t16_comparator sample_t16
    ; make_test "T18" t18_comparator sample_t18
    ]
