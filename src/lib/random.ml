module R = PRNG.Splitmix.Pure

module Seed = struct
  type t = R.t

  let make n = R.make [| n |]
  let make_self_init () = R.make_self_init ()
  let split = R.split
end

type 'a t = { run : Seed.t -> 'a * Seed.t }

let run seed { run } = run seed
let eval s r = fst @@ run s r
let make run = { run }
let int32 = make @@ fun s -> R.int32 Int32.max_int s
let seed = make Seed.split
let return x = { run = (fun s -> (x, s)) }

let generate ~init f =
  make (fun seed ->
    let s1, s2 = Seed.split seed in
    let accum (seed, t) =
      let r = f t in
      let (x, t), s = run seed r in
      Some (x, (s, t))
    in
    let seq = Seq.unfold accum (s1, init) in
    (seq, s2))

let bind { run } f =
  make (fun s ->
    let x, s = run s in
    let { run } = f x in
    run s)

let map f { run } =
  make (fun s ->
    let x, s = run s in
    (f x, s))

let delayed f = make (fun seed -> run seed @@ f ())

let time f x =
  let start = Unix.gettimeofday () in
  let res = f x in
  let stop = Unix.gettimeofday () in
  (res, stop -. start)

let timed { run } =
  make (fun seed ->
    let (x, s), t = time run seed in
    ((x, t), s))

module Syntax = struct
  let ( let* ) = bind
  let ( let+ ) x f = map f x
end

let range mn mx =
  let open Syntax in
  if mx <= mn then
    return mn
  else
    let+ n = int32 in
    let x = Int32.to_int n mod (mx - mn) in
    mn + x

let rec sequence rs =
  let open Syntax in
  match rs with
  | [] -> return []
  | r :: rs ->
    let* x = r in
    let* xs = sequence rs in
    return (x :: xs)

let choose opts =
  let sum = List.fold_left (fun s (fr, _) -> s + fr) 0 opts in
  make
  @@ fun s ->
  let rand_float, s = R.int sum s in
  let rec aux acc = function
    | [ (_, r) ] -> r
    | (f, r) :: frs ->
      let acc = acc + f in
      if acc >= rand_float then
        r
      else
        aux acc frs
    | [] -> failwith "Empty"
  in
  run s @@ aux 0 opts
