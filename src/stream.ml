type tag = Sign | Function | Char | Int | Float | Bool | Value | Operator
[@@deriving show]

type consumed = { tag : tag; data : int32 list } [@@deriving show]

module IM = Map.Make (Int)

type t = { map : int32 IM.t; operators : int array; values : int array }

let make cs =
  let data =
    cs
    |> List.concat_map (fun { tag; data } ->
         List.map (fun d -> (tag, d)) data)
    |> List.mapi (fun ix (tag, d) -> (ix, tag, d))
  in
  let map =
    data |> List.map (fun (ix, _, d) -> (ix, d)) |> List.to_seq |> IM.of_seq
  in
  let operators =
    List.filter_map
      (fun (ix, tag, _) -> if tag = Operator then Some ix else None)
      data
    |> Array.of_list
  in
  let values =
    List.filter_map
      (fun (ix, tag, _) -> match tag with Operator -> None | _ -> Some ix)
      data
    |> Array.of_list
  in
  { map; operators; values }

let shrink_int32 n = Int32.div n 2l

let shrink_operator { map; operators; values } =
  let open Random.Syntax in
  let* op_ix = Random.range 0 @@ Array.length operators in
  let ix = Array.get operators op_ix in
  let n = shrink_int32 @@ IM.find ix map in
  let map = IM.add ix n map in
  Random.return { map; operators; values }

let shrink_value { map; operators; values } =
  let open Random.Syntax in
  let* val_ix = Random.range 0 @@ Array.length values in
  let ix = Array.get values val_ix in
  let n = shrink_int32 @@ IM.find ix map in
  let map = IM.add ix n map in
  Random.return { map; operators; values }

let shrink_one s =
  let open Random.Syntax in
  let* n = Random.range 0 10 in
  if n > 2 then shrink_value s else shrink_operator s

let shrink t =
  Random.make (fun seed ->
    let s1, s2 = PRNG.Splitmix.Pure.split seed in
    let accum (seed, t) =
      let r = shrink_one t in
      let t, s = Random.run seed r in
      Some (t, (s, t))
    in
    let seq = Seq.unfold accum (s1, t) in
    (seq, s2))

let to_seq { map; _ } =
  Seq.unfold
    (fun ix ->
      match IM.find_opt ix map with
      | Some v -> Some (v, ix + 1)
      | None -> None)
    0

let to_list d = to_seq d |> List.of_seq

let to_seq { map; _ } =
  Seq.unfold
    (fun ix ->
      let ix = ix + 1 in
      match IM.find_opt ix map with
      | Some v -> Some (v, ix)
      | None -> Some (0l, ix))
    0

(*
Operators : [4; 2;8; 11; 5]
Values : [[Int, 1); (Float, 3)]]
data : [d1;d2;d4;d4]
overides : [8 -> d8']
*)
