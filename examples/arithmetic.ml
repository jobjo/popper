open Popper
open Sample
open Sample.Syntax

let simple_sum x y = x + y
let simple_diff x y = x - y
let simple_div x y = if y = 0 then None else Some (x / y)

type point =
  { x : int
  ; y : int
  }
[@@deriving show, ord, popper]

let point_sum p1 p2 : point =
  { x = simple_sum p1.x p2.x; y = simple_sum p1.y p2.y }

let test_sum =
  test (fun () ->
    (* Getting two built-in samples *)
    let* left = int in
    let* right = int in
    let expected = left + right in
    let actual = simple_sum left right in
    equal Comparator.int actual expected)

let test_diff =
  test (fun () ->
    (* Getting two built-in samples *)
    let* left = int in
    let* right = int in
    let expected = left - right in
    let actual = simple_diff left right in
    (* Use built in sample and comparator *)
    let comparator = Comparator.int in
    equal comparator actual expected)

let test_point_sum =
  test (fun () ->
    let* left = point_sample in
    let* right = point_sample in
    let expected = { x = left.x + right.x; y = left.y + right.y } in
    let actual = point_sum left right in
    equal ~loc:__LOC__ point_comparator expected actual)

let test_division =
  test (fun () ->
    let* x = Int.range (-10) 10 in
    let* y = Int.range (-10) 10 in
    let expected = if y = 0 then None else Some (x / y) in
    let actual = simple_div x y in
    let comparator = Comparator.option Comparator.int in
    equal comparator actual expected)

let suite =
  suite
    [ ("Sum", test_sum)
    ; ("Diff", test_diff)
    ; ("Point Sum", test_point_sum)
    ; ("Divide", test_division)
    ]
