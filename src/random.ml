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
let return x = { run = (fun s -> x, s) }

let generate ~init f =
  make (fun seed ->
    let s1, s2 = Seed.split seed in
    let accum (seed, t) =
      let r = f t in
      let (x, t), s = run seed r in
      Some (x, (s, t))
    in
    let seq = Seq.unfold accum (s1, init) in
    seq, s2)

let bind { run } f =
  make (fun s ->
    let x, s = run s in
    let { run } = f x in
    run s)

let map f { run } =
  make (fun s ->
    let x, s = run s in
    f x, s)

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
