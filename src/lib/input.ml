open Random.Syntax

type t =
  { data : int32 Seq.t
  ; max_size : int
  }

let make ~max_size =
  let gen ix =
    if ix <= 10_000 then
      let+ x = Random.int32 in
      x, ix + 1
    else
      Random.return (0l, ix + 1)
  in
  let+ data = Random.generate ~init:0 gen in
  { data; max_size }

let make_seq ~max_size =
  Random.generate ~init:2 (fun size ->
    let max_size = min size max_size in
    let+ x = make ~max_size in
    x, max_size + 1)

let of_list ~max_size xs = { max_size; data = List.to_seq xs }
let of_seq ~max_size data = { max_size; data }

let head_tail { max_size; data } =
  match data () with
  | Seq.Nil -> None
  | Cons (n, data) -> Some (n, { max_size; data })

let head input = Option.map fst @@ head_tail input
let take n { data; _ } = List.of_seq @@ Containers.Seq.take n data
let drop n { max_size; data } = { max_size; data = Containers.Seq.drop n data }
let map f { max_size; data } = { max_size; data = Seq.map f data }
let max_size { max_size; _ } = max_size
let set_max_size max_size { data; _ } = { data; max_size }
