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

let shrink_operator t =
  if Array.length t.operators = 0 then
    Random.return t
  else
    let* ix = Random.range 0 @@ Array.length t.operators in
    let ix = Array.get t.operators ix in
    let n = shrink_int32 @@ IM.find ix t.map in
    let map = IM.add ix n t.map in
    Random.return { t with map }

let shrink_value t =
  if Array.length t.values = 0 then
    Random.return t
  else
    let* ix = Random.range 0 @@ Array.length t.values in
    let ix = Array.get t.values ix in
    let n = shrink_int32 @@ IM.find ix t.map in
    let map = IM.add ix n t.map in
    Random.return { t with map }

let shrink_one t =
  let* n = Random.range 0 10 in
  if n >= 2 then shrink_value t else shrink_operator t

let shrink t =
  let+ ts =
    Random.generate ~init:t (fun t ->
      let+ t = shrink_one t in
      (t, t))
  in
  Seq.uniq ( = ) @@ Seq.take 1000 ts

let to_input { map; _ } =
  Input.of_seq
    (Seq.unfold
       (fun ix ->
         let v =
           match IM.find_opt ix map with
           | Some v -> v
           | None -> 0l
         in
         Some (v, ix + 1))
       0)

let shrink output =
  let open Random.Syntax in
  let+ ts = shrink @@ of_output output in
  Seq.map to_input ts

let find_next prop output =
  let open Random.Syntax in
  let rec aux ix =
    if ix > 100 then
      Random.return None
    else
      let* output =
        let+ inputs = shrink output in
        inputs
        |> Seq.take 1000
        |> Seq.filter_map (fun input ->
             let output = Generator.run input prop in
             if Proposition.is_fail @@ Output.value output then
               Some output
             else
               None)
        |> Seq.head
      in
      match output with
      | Some output -> Random.return (Some output)
      | None -> aux (ix + 1)
  in
  aux 0

let shrink ~max_count output prop =
  let open Random.Syntax in
  let rec aux ~ix ~num_unique output =
    if ix >= max_count then
      match Output.value output with
      | Proposition.Fail pp -> Random.return @@ Some (num_unique, pp)
      | _ -> Random.return None
    else
      match Output.value output with
      | Proposition.Fail _ -> (
        let* output_opt = find_next prop output in
        match output_opt with
        | Some new_output ->
          let num_unique =
            let output_matches =
              Output.consumed output = Output.consumed new_output
            in
            num_unique + if output_matches then 0 else 1
          in
          aux ~ix:(ix + 1) ~num_unique new_output
        | None -> Random.return None)
      | _ -> Random.return None
  in
  aux ~ix:0 ~num_unique:0 output
