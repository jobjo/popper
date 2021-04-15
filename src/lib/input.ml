open Random.Syntax

type t =
  { data : int32 Seq.t
  ; size : int
  }

let make ~size =
  let gen ix =
    if ix <= 10_000 then
      let+ x = Random.int32 in
      (x, ix + 1)
    else
      Random.return (0l, ix + 1)
  in
  let+ data = Random.generate ~init:0 gen in
  { data; size }

let make_seq ~size =
  Random.generate ~init:2 (fun s ->
    let size = min size s in
    let+ x = make ~size in
    (x, size + 1))

let of_seq ~size data =
  let zeros = Seq.unfold (fun _ -> Some (0l, ())) () in
  let data = Seq.append data zeros in
  { size; data }

let of_list ~size xs = of_seq ~size (List.to_seq xs)

let head_tail { size; data } =
  match data () with
  | Seq.Nil -> None
  | Cons (n, data) -> Some (n, { size; data })

let head input = Option.map fst @@ head_tail input
let take n { data; _ } = List.of_seq @@ Containers.Seq.take n data
let drop n { size; data } = { size; data = Containers.Seq.drop n data }
let map f { size; data } = { size; data = Seq.map f data }
let size { size; _ } = size
let set_size size { data; _ } = { data; size }
