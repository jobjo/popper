open Popper
open Generator.Syntax

type t =
  { x_values : int list
  ; y_values : int list
  ; x_axis : string
  ; y_axis : string
  }
[@@deriving show]

let flip { x_values; y_values; x_axis; y_axis } =
  { x_values = y_values; y_values = x_values; x_axis = y_axis; y_axis = x_axis }

let gen =
  let open Generator in
  let* num_values = range 1 100 in
  let* x_axis = string in
  let* y_axis = string in
  let* x_values = sequence @@ List.init num_values (fun _ -> int) in
  let* y_values = sequence @@ List.init num_values (fun _ -> int) in
  let chart = { x_values; y_values; x_axis; y_axis } in
  return chart

let test_flip =
  Test.test
    ~count:1000
    (let* s = gen in
     Test.equal pp (flip @@ flip s) s)

let suite = Test.suite [ "Flip series", test_flip ]
