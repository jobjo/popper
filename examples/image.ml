module Image = struct
  type color =
    | White
    | Black
    | Transparent

  type t =
    { width : int
    ; height : int
    ; get_pixel : x:int -> y:int -> color
    }

  let make ~width ~height get_pixel = { width; height; get_pixel }

  let render { width; height; get_pixel } =
    List.init height (fun y ->
      List.init width (fun x ->
        match get_pixel ~x ~y with
        | White -> "w "
        | Black -> "b "
        | Transparent -> "- ")
      |> String.concat "")
    |> String.concat "\n"

  let transpose { width; height; get_pixel } =
    { width = height
    ; height = width
    ; get_pixel = (fun ~x ~y -> get_pixel ~x:y ~y:x)
    }

  let invert { width; height; get_pixel } =
    let get_pixel ~x ~y =
      match get_pixel ~x ~y with
      | White -> Black
      | Black -> White
      | Transparent -> White
    in
    { width; height; get_pixel }

  let next
    { width = w1; height = h1; get_pixel = g1 }
    { width = w2; height = h2; get_pixel = g2 }
    =
    let width = w1 + w2 in
    let height = max h1 h2 in
    let get_pixel ~x ~y =
      if x < w1 then
        if y < h1 then
          g1 ~x ~y
        else
          Transparent
      else if y < h2 then
        g2 ~x:(x - w1) ~y
      else
        Transparent
    in
    { width; height; get_pixel }

  let above i1 i2 = transpose @@ next (transpose i1) (transpose i2)
end

open Popper
open Sample.Syntax

let sample_img =
  let* width = Sample.Int.range 0 10 in
  let* height = Sample.Int.range 0 10 in
  let* lookup =
    Sample.fn
      (Sample.one_value_of [ Image.Black; Image.White; Image.Transparent ])
  in
  let get_pixel ~x ~y = lookup (x, y) in
  Sample.return (Image.make ~width ~height get_pixel)

let test_invert_twice =
  test ~config:Config.(all [ num_samples 100; verbose ]) @@ fun () ->
  let* img = sample_img in
  equal
    Comparator.string
    (Image.render img)
    (Image.render @@ Image.invert @@ Image.invert img)

let suite = suite [ ("Invert twice", test_invert_twice) ]
