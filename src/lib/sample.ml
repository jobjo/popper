type 'a t = { gen : Input.t -> 'a Output.t }

let run input { gen } = gen input
let make gen = { gen }
let tag tag gen = make (fun input -> Output.tag tag @@ run input gen)
let map f gen = make (fun input -> Output.map f @@ run input gen)

let return value =
  make (fun input ->
    Output.make ~value ~consumed:Consumed.empty ~remaining:input ~log:Log.empty)

let bind s f =
  make (fun input ->
    let output1 = run input s in
    let input2 = Output.remaining output1 in
    let output2 = run input2 (f @@ Output.value output1) in
    let consumed =
      Consumed.add (Output.consumed output1) (Output.consumed output2)
    in
    let log = Log.add (Output.log output1) (Output.log output2) in
    output2 |> Output.set_consumed consumed |> Output.set_log log)

let both g1 g2 =
  let ( let* ) = bind in
  let* x = g1 in
  let* y = g2 in
  return (x, y)

let size = make @@ fun input -> run input (return @@ Input.size input)

let resize size s =
  make (fun input ->
    let org_size = Input.size input in
    let output = run (Input.set_size size input) s in
    let remaining = Input.set_size org_size @@ Output.remaining output in
    Output.set_remaining remaining output)

let delayed f = make (fun input -> run input @@ f ())

module Syntax = struct
  let ( let* ) = bind
  let ( let+ ) x f = map f x
  let ( and* ) = both
  let ( and+ ) = both
end

open Syntax

let log log =
  make (fun input ->
    Output.make ~value:() ~consumed:Consumed.empty ~remaining:input ~log)

let log_with pp x =
  let pp out = Format.fprintf out "%a" pp x in
  log @@ Log.of_pp pp

let pp_key_value pp_value out (key, value) =
  Format.fprintf
    out
    "@[<hv 2>%a@;=@;%a@]"
    (Util.Format.yellow Format.pp_print_string)
    key
    (Util.Format.blue pp_value)
    value

let log_key_value key value =
  let lines = String.split_on_char '\n' value in
  let pp_value =
    Util.Format.blue (Format.pp_print_list Format.pp_print_string)
  in
  let pp out = pp_key_value pp_value out in
  log_with pp (key, lines)

let with_log key pp gen =
  let* value = gen in
  let* () = log_with (pp_key_value pp) (key, value) in
  return value

let int32 =
  make (fun input ->
    match Input.head_tail input with
    | None -> failwith "End-of-sequence"
    | Some (value, remaining) ->
      Output.make
        ~value
        ~consumed:(Consumed.value value)
        ~remaining
        ~log:Log.empty)

let rec sequence gs =
  match gs with
  | [] -> return []
  | g :: gs ->
    let* x = g in
    let* xs = sequence gs in
    return (x :: xs)

let range mn mx =
  (* Always read one value *)
  let+ r = int32 in
  if mx <= mn then
    mn
  else
    let n = mx - mn in
    let n = Int32.of_int n in
    let block = Int32.div Int32.max_int n in
    let offset = Int32.div r block in
    mn + Int32.to_int offset

let one_of gs =
  let* n = range 0 (List.length gs) in
  List.nth gs n

let tot_float_range = Int32.to_float Int32.max_int -. Int32.to_float 0l

let float_range mn mx =
  let+ r = int32 in
  let f = Int32.to_float r in
  let range = mx -. mn in
  let g = f /. tot_float_range in
  mn +. (g *. range)

let choose opts =
  let sum = List.fold_left (fun s (fr, _) -> s +. fr) 0. opts in
  let* rand = tag Tag.Choice @@ float_range 0. sum in
  let rec aux acc = function
    | [ (_, r) ] -> r
    | (f, r) :: frs ->
      let acc = acc +. f in
      if acc >= rand then
        r
      else
        aux acc frs
    | [] -> failwith "Empty"
  in
  aux 0. opts

let sized f = make @@ fun input -> run input @@ f @@ Input.size input

let list g =
  let rec aux size =
    let list () =
      let size = size / 2 in
      let* x = tag (Tag.Name "element") @@ resize size g in
      let* xs = aux size in
      let* ys = aux size in
      return (x :: xs @ ys)
    in
    tag Tag.Sub_list @@ choose [ (1., return []); (float size, delayed list) ]
  in
  tag Tag.List @@ sized aux

let option g =
  sized (fun size ->
    if size <= 1 then
      return None
    else
      one_of [ return None; map Option.some g ])

let result ~ok ~error = one_of [ map Result.ok ok; map Result.error error ]
let unit = return ()
let one_value_of vs = one_of @@ List.map return vs

let promote f =
  make (fun input ->
    let value x = Output.value @@ run input @@ f x in
    let consumed =
      Consumed.tag Tag.Function @@ Consumed.value (List.hd @@ Input.take 1 input)
    in
    let remaining = Input.drop 1 input in
    Output.make ~value ~consumed ~remaining ~log:Log.empty)

let bool = tag Tag.Bool @@ one_value_of [ false; true ]

let fn g =
  let f x =
    make (fun input ->
      let hash = Hashtbl.hash x in
      let modify x =
        let seed = Random.Seed.make [ Int32.to_int x; hash ] in
        Random.eval seed Random.int32
      in
      let data = Input.map modify input in
      run data g)
  in
  promote f

let with_consumed g =
  make (fun input ->
    let output = run input g in
    let c = Output.consumed output in
    Output.map (fun x -> (x, c)) output)

let tag_name name = tag (Tag.Name name)

module Int = struct
  let range = range
  let small = range (-10) 10
  let medium = range (-1000) 1000

  let any_int =
    let* b = tag Sign int32 in
    let* n = tag Int int32 in
    let n = Int32.to_int n in
    let b = Int32.to_int b mod 2 <> 0 in
    return (if b then Stdlib.Int.neg n else n)

  let int =
    tag Int
    @@ choose
         [ (5., return 0)
         ; (10., return 1)
         ; (10., return (-1))
         ; (50., small)
         ; (100., medium)
         ; (100., any_int)
         ; (2., return Int.max_int)
         ; (2., return Int.min_int)
         ]

  let positive =
    let to_pos n =
      if n <= 0 then
        let an = abs n in
        if an > 0 then
          an
        else
          1
      else
        n
    in
    map to_pos int

  let negative = map (fun n -> 0 - n) positive
end

module Float = struct
  let range mn mx = float_range mn mx
  let sub_normal = range (-0.000001) 0.000001
  let small = range (-10.) 10.
  let medium = range (-1000.) 1000.
  let positive = float_range 0. Float.max_float
  let negative = map Float.neg positive

  let any =
    tag Tag.Float
    @@ let+ n = tag Float int32 in
       Int32.float_of_bits n

  let float =
    choose
      [ (1., return 0.)
      ; (1., sub_normal)
      ; (40., small)
      ; (40., medium)
      ; (15., any)
      ; (1., return Float.nan)
      ; (1., return Float.infinity)
      ; (1., return Float.neg_infinity)
      ]
end

module List = struct
  let of_length n g =
    let* size = size in
    let g =
      if n <= 0 then
        g
      else
        resize (size / n) g
    in
    sequence @@ List.init n (fun _ -> g)

  let range mn mx g =
    let* n = Int.range mn mx in
    of_length n g

  let non_empty g =
    let+ x = g
    and+ xs = list g in
    x :: xs
end

module Array = struct
  let of_length n g = map Array.of_list @@ List.of_length n g
  let range mn mx g = map Array.of_list @@ List.range mn mx g
  let non_empty g = map Array.of_list @@ List.non_empty g
end

module Char = struct
  let upper = map Char.chr @@ Int.range 65 91
  let lower = map Char.chr @@ Int.range 97 123
  let numeric = map Char.chr @@ Int.range 48 58
  let alpha = choose [ (3., lower); (1., upper) ]
  let alpha_numeric = choose [ (5., lower); (2., upper); (1., numeric) ]
  let any_char = map Char.chr @@ Int.range 0 256

  let char =
    choose
      [ (1., return 'a')
      ; (10., lower)
      ; (5., upper)
      ; (5., alpha_numeric)
      ; (1., any_char)
      ]
end

module String = struct
  let string =
    map
      (fun input -> String.concat "" @@ Stdlib.List.map (String.make 1) input)
      (list Char.char)

  let of_list cs = String.of_seq @@ Stdlib.List.to_seq cs
  let of_length n = map (String.concat "") @@ List.of_length n string
  let range mn mx = map (String.concat "") @@ List.range mn mx string
  let numeric = map of_list @@ list Char.numeric
  let alpha_numeric = map of_list @@ list Char.alpha_numeric
  let alpha = map of_list @@ list Char.alpha
  let upper = map of_list @@ list Char.upper
  let lower = map of_list @@ list Char.lower
end

module Tuple = struct
  let pair g1 g2 =
    let* size = size in
    let size = size / 2 in
    let* x = resize size g1 in
    let* y = resize size g2 in
    return (x, y)

  let tripple g1 g2 g3 =
    let* size = map (fun s -> s / 3) size in
    let* x = resize size g1 in
    let* y = resize size g2 in
    let* z = resize size g3 in
    return (x, y, z)

  let quad g1 g2 g3 g4 =
    let* size = map (fun s -> s / 4) size in
    let* x = resize size g1 in
    let* y = resize size g2 in
    let* z = resize size g3 in
    let* u = resize size g4 in
    return (x, y, z, u)
end

let int = Int.int
let float = Float.float

let int64 =
  let+ f = float in
  Int64.of_float f

let string = String.string
let char = Char.char
