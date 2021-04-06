open Popper
open Generator.Syntax

type t =
  { x_values : int list
  ; y_values : int list
  ; x_axis : string
  ; y_axis : string
  }
[@@deriving show, popper]

let flip { x_values; y_values; x_axis; y_axis } =
  { x_values = y_values; y_values = x_values; x_axis = y_axis; y_axis = x_axis }

let comparator = Comparator.make ( = ) pp

let test_flip =
  Test.test ~count:1000 (fun () ->
    let* s = generate in
    Test.equal comparator (flip s) s)

let suite = Test.suite [ ("Flip series", test_flip) ]
