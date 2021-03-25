module R = PRNG.Splitmix.Pure

type 'a t = { run : R.t -> 'a * R.t }

let run seed { run } = run seed

let run_self_init r = fst @@ run (R.make_self_init ()) r

let make run = { run }

let return x = { run = (fun s -> (x, s)) }

let range mn mx =
  make (fun s ->
    let x, s = R.int (mx - mn) s in
    (mn + x, s))

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
end
