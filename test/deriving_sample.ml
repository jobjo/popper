type t1 =
  | A
  | B
[@@deriving sample]

type t2 =
  | A of int
  | B of t2
[@@deriving sample]

type t3 =
  [ `True
  | `False
  ]
[@@deriving sample]

type t4 =
  [ `Text of string list
  | `Div of t4
  ]
[@@deriving sample]

type t5 =
  { s : string
  ; i : int
  }
[@@deriving sample]

type t6 =
  { x : string option list
  ; y : int
  ; z : (bool list, string) result list
  }
[@@deriving sample]

type t7 =
  | A7 of
      { s : string
      ; i : int
      }
  | B7
[@@deriving sample]

type t8 = int [@@deriving sample]

type t9 =
  | Leaf of int
  | Node of t10

and t10 =
  { value : int
  ; left : t9
  ; right : t9
  }
[@@deriving sample]

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
[@@deriving sample]

and t18 = int t17 [@@deriving sample]

type t19 = { fn : int -> bool } [@@deriving sample]

let make_test name sample =
  let open Popper in
  let open Popper.Syntax in
  ( name
  , test (fun () ->
      let* _ = sample in
      is_true true) )

let suite =
  Popper.Test.suite
    [ make_test "T1" sample_t1
    ; make_test "T2" sample_t2
    ; make_test "T3" sample_t3
    ; make_test "T4" sample_t4
    ; make_test "T5" sample_t5
    ; make_test "T6" sample_t6
    ; make_test "T7" sample_t7
    ; make_test "T8" sample_t8
    ; make_test "T9" sample_t9
    ; make_test "T10" sample_t10
    ; make_test "T12" sample_t12
    ; make_test "T13" sample_t13
    ; make_test "T14" sample_t14
    ; make_test "T16" sample_t16
    ; make_test "T18" sample_t18
    ; make_test "T19" sample_t18
    ]
