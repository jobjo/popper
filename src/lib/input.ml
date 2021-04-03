open Random.Syntax

type t = int32 Seq.t

let make =
  let gen () =
    let+ x = Random.int32 in
    x, ()
  in
  Random.generate ~init:() gen

let make_seq =
  Random.generate ~init:() (fun () ->
    let+ x = make in
    x, ())

let of_list xs = Containers.Seq.of_list xs
let of_seq t = t

let head_tail input =
  match input () with
  | Seq.Nil -> None
  | Cons (n, rest) -> Some (n, rest)

let head input = Option.map fst @@ head_tail input
let take n input = List.of_seq @@ Containers.Seq.take n input
let drop n = Containers.Seq.drop n
let map f = Seq.map f
