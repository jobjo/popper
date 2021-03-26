type 'a t = { gen : Input.t -> 'a Output.t }

let run input { gen } = gen input
let make gen = { gen }
let tag tag gen = make (fun input -> Output.tag tag @@ run input gen)
let map f gen = make (fun input -> Output.map f @@ run input gen)

let return value =
  make (fun input -> Output.make ~value ~consumed:[] ~remaining:input)

let bind gen f =
  make (fun input ->
    let o1 = run input gen in
    let o2 = run (Output.remaining o1) (f @@ Output.value o1) in
    Output.set_consumed (Output.consumed o1 @ Output.consumed o2) o2)

let both g1 g2 =
  let ( let* ) = bind in
  let* x = g1 in
  let* y = g2 in
  return (x, y)

let int32 =
  make (fun input ->
    match Input.head_tail input with
    | None -> failwith "End-of-sequence"
    | Some (value, remaining) ->
      Output.make
        ~value
        ~consumed:[ Consumed.make Tag.Value [ value ] ]
        ~remaining)

module Syntax = struct
  let ( let* ) = bind
  let ( let+ ) x f = map f x
  let ( and* ) = both
  let ( and+ ) = both
end

open Syntax

let rec sequence gs =
  match gs with
  | [] -> return []
  | g :: gs ->
    let* x = g in
    let* xs = sequence gs in
    return (x :: xs)

let many g =
  let* n = tag Operator @@ map Int32.to_int int32 in
  sequence @@ List.init (n mod 10) (fun _ -> g)

let range mn mx =
  let+ n = tag Int int32 in
  mn + (Int32.to_int n mod (mx - mn))

let one_of gs =
  let* n = range 0 (List.length gs) in
  List.nth gs n

let char =
  let+ n = tag Char @@ map Int32.to_int int32 in
  Char.chr (48 + (n mod (122 - 48)))

let one_value_of vs = one_of @@ List.map return vs

let promote f =
  make (fun input ->
    let value x = Output.value @@ run input @@ f x in
    let consumed = [ Consumed.make Tag.Function (Input.take 1 input) ] in
    let remaining = Input.drop 1 input in
    Output.make ~value ~consumed ~remaining)

let float =
  let+ n = tag Float int32 in
  Int32.float_of_bits n

let int64 =
  let+ f = float in
  Int64.of_float f

let bool = one_value_of [ false; true ]

let arrow g =
  let f x =
    make (fun input ->
      let () =
        match Input.head input with
        | Some x -> Printf.printf "Value %d\n" (Int32.to_int x)
        | None -> ()
      in
      let h = Int32.of_int @@ Hashtbl.hash x in
      let data = Input.map (Int32.logxor h) input in
      let () =
        match Input.head data with
        | Some x -> Printf.printf "Data value %d\n" (Int32.to_int x)
        | None -> ()
      in
      run data g)
  in
  promote f

let int =
  let* b = tag Sign int32 in
  let* n = tag Int int32 in
  let n = Int32.to_int n in
  let b = Int32.to_int b mod 2 = 0 in
  return (if b then Int.neg n else n)

let string =
  map
    (fun input -> String.concat "" @@ List.map (String.make 1) input)
    (many char)
