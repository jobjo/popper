type 'a t = { gen : Input.t -> 'a Output.t }

let run input { gen } = gen input
let make gen = { gen }
let tag tag gen = make (fun input -> Output.tag tag @@ run input gen)
let map f gen = make (fun input -> Output.map f @@ run input gen)

let return value =
  make (fun input ->
    Output.make ~value ~consumed:Consumed.empty ~remaining:input ~log:Log.empty)

let bind gen f =
  make (fun input ->
    let o1 = run input gen in
    let o2 = run (Output.remaining o1) (f @@ Output.value o1) in
    let consumed = Consumed.add (Output.consumed o1) (Output.consumed o2) in
    let log = Log.add (Output.log o1) (Output.log o2) in
    o2 |> Output.set_consumed consumed |> Output.set_log log)

let both g1 g2 =
  let ( let* ) = bind in
  let* x = g1 in
  let* y = g2 in
  return (x, y)

let delayed f = make (fun input -> run input @@ f ())

let log log =
  make (fun input ->
    Output.make ~value:() ~consumed:Consumed.empty ~remaining:input ~log)

let log_string s =
  let pp out = Format.pp_print_string out s in
  log @@ Log.of_pp pp

let log_with pp x =
  let pp out = Format.fprintf out "%a" pp x in
  log @@ Log.of_pp pp

let log_key_value key value =
  let pp out () =
    Format.fprintf
      out
      "%a = %a"
      (Printer.yellow Format.pp_print_string)
      key
      (Printer.blue Format.pp_print_string)
      value
  in
  log_with pp ()

let int32 =
  make (fun input ->
    match Input.head_tail input with
    | None -> failwith "End-of-sequence"
    | Some (value, remaining) ->
      Output.make
        ~value
        ~consumed:(Consumed.make [ value ])
        ~remaining
        ~log:Log.empty)

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

let range mn mx =
  let+ n = tag Int int32 in
  mn + (Int32.to_int n mod (mx - mn))

let one_of gs =
  let* n = tag Tag.Operator @@ range 0 (List.length gs) in
  List.nth gs n

let sized f =
  let* n = tag Tag.Size int32 in
  let max_size = 100l in
  let block = Int32.div Int32.max_int max_size in
  let n = Int32.to_int @@ Int32.div n block in
  f n

let list g =
  let rec aux size =
    if size <= 1 then
      return []
    else
      let f () =
        let+ x = g
        and+ xs = aux (size - 1) in
        x :: xs
      in
      one_of [ return []; delayed f ]
  in
  sized aux

let option g =
  sized (fun size ->
    if size <= 1 then
      return None
    else
      one_of [ return None; map Option.some g ])

let result ~ok ~error = one_of [ map Result.ok ok; map Result.error error ]

let char =
  let+ n = tag Char @@ map Int32.to_int int32 in
  Char.chr (48 + (n mod (122 - 48)))

let one_value_of vs = one_of @@ List.map return vs

let promote f =
  make (fun input ->
    let value x = Output.value @@ run input @@ f x in
    let consumed =
      Consumed.tag Tag.Function @@ Consumed.make (Input.take 1 input)
    in
    let remaining = Input.drop 1 input in
    Output.make ~value ~consumed ~remaining ~log:Log.empty)

let float =
  tag Tag.Float
  @@ let+ n = tag Float int32 in
     Int32.float_of_bits n

let int64 =
  let+ f = float in
  Int64.of_float f

let bool = one_value_of [ false; true ]

let arrow g =
  let f x =
    make (fun input ->
      let h = Int32.of_int @@ Hashtbl.hash x in
      let data = Input.map (Int32.logxor h) input in
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
    (list char)

let with_consumed g =
  make (fun input ->
    let output = run input g in
    let c = Output.consumed output in
    Output.map (fun x -> x, c) output)
