open Popper

type t =
  { x_values : int list
  ; y_values : int list
  ; x_axis : string
  ; y_axis : string
  }
[@@deriving show, ord, popper]

let flip { x_values; y_values; x_axis; y_axis } =
  { x_values = y_values; y_values = x_values; x_axis = y_axis; y_axis = x_axis }

let test_flip =
  let open Sample.Syntax in
  test (fun () ->
    let* s = sample in
    Popper.equal comparator (flip s) s)

let suite = Popper.suite [ ("Flip chart", test_flip) ]
