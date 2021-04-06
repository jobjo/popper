open Popper
open Generator
open Syntax
open Format

let simple_sum x y = x + y
let simple_diff x y = x - y
let simple_div x y = if y = 0 then None else Some (x / y)

type point =
  { x : int
  ; y : int
  }
[@@deriving show, eq, popper]

let point_sum p1 p2 : point =
  { x = simple_sum p1.x p2.x; y = simple_sum p1.y p2.y }

let test_sum =
  Test.test (fun () ->
    (* Getting two built-in generators *)
    let* left = int in
    let* right = int in
    let expected = left + right in
    let actual = simple_sum left right in
    (* Comparator and pretty printer *)
    let int_comparator x y = x = y in
    let int_pretty_printer = pp_print_int in
    let comparator = Comparator.make int_comparator int_pretty_printer in
    Test.equal comparator actual expected)

let test_diff =
  Test.test (fun () ->
    (* Getting two built-in generators *)
    let* left = int in
    let* right = int in
    let expected = left - right in
    let actual = simple_diff left right in
    (* Use built in generator and comparator *)
    let comparator = Comparator.int in
    Test.equal comparator actual expected)

let test_point_sum =
  Test.test (fun () ->
    let* left = generate_point in
    let* right = generate_point in
    let expected = { x = left.x + right.x; y = left.y + right.y } in
    let actual = point_sum left right in
    Test.equal ~loc:__LOC__ point_comparator expected actual)

let test_division =
  Test.test (fun () ->
    let* x = range (-10) 10 in
    let* y = range (-10) 10 in
    let expected = if y = 0 then None else Some (x / y) in
    let actual = simple_div x y in
    let comparator = Comparator.option Comparator.int in
    Test.equal comparator actual expected)

let suite =
  Test.suite
    [ ("Sum", test_sum)
    ; ("Diff", test_diff)
    ; ("Point Sum", test_point_sum)
    ; ("Divide", test_division)
    ]
