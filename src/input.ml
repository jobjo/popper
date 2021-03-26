type t = int32 Seq.t

let make seed =
  let accum s =
    let n, s = Random.run s @@ Random.int32 in
    Some (n, s)
  in
  Seq.unfold accum seed

let make_seq seed =
  let accum s =
    let s1, s2 = Random.split_seed s in
    Some (make s1, s2)
  in
  Seq.unfold accum seed

let of_list xs = Containers.Seq.of_list xs

let of_seq t = t

let make_self_init () = make @@ Random.make_seed_self_init ()

let make_seq_self_init () = make_seq @@ Random.make_seed_self_init ()

let make_seq seed = make_seq seed

let head_tail input =
  match input () with
  | Seq.Nil -> None
  | Cons (n, rest) -> Some (n, rest)

let head input = Option.map fst @@ head_tail input

let take n input = List.of_seq @@ Containers.Seq.take n input

let drop n = Containers.Seq.drop n

let map f = Seq.map f
