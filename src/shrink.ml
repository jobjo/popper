open Random.Syntax
module Seq = Containers.Seq
module IM = Map.Make (Int)

type t = { map : int32 IM.t; operators : int array; values : int array }

let of_output output =
  let data =
    Output.consumed output
    |> List.concat_map (fun cons ->
         List.map (fun d -> (Consumed.tag cons, d)) @@ Consumed.data cons)
    |> List.mapi (fun ix (tag, d) -> (ix, tag, d))
  in
  let map =
    data |> List.map (fun (ix, _, d) -> (ix, d)) |> List.to_seq |> IM.of_seq
  in
  let operators =
    List.filter_map
      (fun (ix, tag, _) -> if Tag.is_operator tag then Some ix else None)
      data
    |> Array.of_list
  in
  let values =
    List.filter_map
      (fun (ix, tag, _) -> if Tag.is_value tag then Some ix else None)
      data
    |> Array.of_list
  in
  { map; operators; values }

let shrink_int32 n = Int32.div n 2l

let shrink_operator { map; operators; values } =
  let* op_ix = Random.range 0 @@ Array.length operators in
  let ix = Array.get operators op_ix in
  let n = shrink_int32 @@ IM.find ix map in
  let map = IM.add ix n map in
  Random.return { map; operators; values }

let shrink_value { map; operators; values } =
  let* val_ix = Random.range 0 @@ Array.length values in
  let ix = Array.get values val_ix in
  let n = shrink_int32 @@ IM.find ix map in
  let map = IM.add ix n map in
  Random.return { map; operators; values }

let shrink_one t =
  let* n = Random.range 0 10 in
  if n >= 2 then shrink_value t else shrink_operator t

let shrink t =
  let+ ts = Random.generate shrink_one t in
  Seq.take 1000 ts

let to_input { map; _ } =
  Input.of_seq
    (Seq.unfold
       (fun ix ->
         let v = match IM.find_opt ix map with Some v -> v | None -> 0l in
         Some (v, ix + 1))
       0)

let shrink output =
  let open Random.Syntax in
  let+ ts = shrink @@ of_output output in
  Seq.map to_input ts
