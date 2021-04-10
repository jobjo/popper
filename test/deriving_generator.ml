type t1 =
  | A
  | B
[@@deriving generator]

type t2 =
  | A of int
  | B of t2
[@@deriving generator]

type t3 =
  [ `True
  | `False
  ]
[@@deriving generator]

type t4 =
  [ `Text of string list
  | `Div of t4
  ]
[@@deriving generator]

type t5 =
  { s : string
  ; i : int
  }
[@@deriving generator]

type t6 =
  { x : string option list
  ; y : int
  ; z : (bool list, string) result list
  }
[@@deriving generator]

type t7 =
  | A7 of
      { s : string
      ; i : int
      }
  | B7
[@@deriving generator]

type t8 = int [@@deriving generator]

type t9 =
  | Leaf of int
  | Node of t10

and t10 =
  { value : int
  ; left : t9
  ; right : t9
  }
[@@deriving generator]

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

type 'a t17 =
  | Node of 'a t17 * 'a t17
  | Leaf
  | T18 of { t18 : t18 option }
[@@deriving generator]

and t18 = int t17 [@@deriving generator]

type t19 = { fn : int -> bool } [@@deriving generator]

let make_test name generator =
  let open Popper in
  let open Popper.Syntax in
  ( name
  , test (fun () ->
      let* _ = generator in
      is_true true) )

let suite =
  Popper.Test.suite
    [ make_test "T1" generate_t1
    ; make_test "T2" generate_t2
    ; make_test "T3" generate_t3
    ; make_test "T4" generate_t4
    ; make_test "T5" generate_t5
    ; make_test "T6" generate_t6
    ; make_test "T7" generate_t7
    ; make_test "T8" generate_t8
    ; make_test "T9" generate_t9
    ; make_test "T10" generate_t10
    ; make_test "T12" generate_t12
    ; make_test "T13" generate_t13
    ; make_test "T14" generate_t14
    ; make_test "T16" generate_t16
    ; make_test "T18" generate_t18
    ; make_test "T19" generate_t18
    ]
