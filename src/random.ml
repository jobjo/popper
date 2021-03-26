module R = PRNG.Splitmix.Pure

type seed = R.t

type 'a t = { run : seed -> 'a * seed }

let make_seed n = R.make [| n |]

let make_seed_self_init () = R.make_self_init ()

let split_seed = R.split

let run seed { run } = run seed

let eval s r = fst @@ run s r

let run_self_init r = fst @@ run (R.make_self_init ()) r

let make run = { run }

let int32 = make @@ fun s -> R.int32 Int32.max_int s

let seed = make split_seed

let return x = { run = (fun s -> (x, s)) }

let generate f zero =
  make (fun seed ->
    let s1, s2 = split_seed seed in
    let accum (seed, t) =
      let r = f t in
      let t, s = run seed r in
      Some (t, (s, t))
    in
    let seq = Seq.unfold accum (s1, zero) in
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
