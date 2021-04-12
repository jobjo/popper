open Random.Syntax
module IM = Map.Make (Int)

type t =
  { map : int32 option IM.t
  ; operators : int array
  ; values : int array
  ; max_size : int
  }

type result =
  { num_shrinks : int
  ; num_attempts : int
  ; pp : Format.formatter -> unit -> unit
  ; output : Proposition.t Output.t
  }

let of_output output =
  let data =
    Consumed.to_list @@ Output.consumed output
    |> List.mapi (fun ix (tag, d) -> (ix, tag, d))
  in
  let map =
    data
    |> List.map (fun (ix, _, d) -> (ix, Some d))
    |> List.to_seq
    |> IM.of_seq
  in
  let operators =
    List.filter_map
      (fun (ix, tags, _) ->
        match List.rev tags with
        | [] -> None
        | tag :: _ -> if Tag.is_operator tag then Some ix else None)
      data
    |> Array.of_list
  in
  let values =
    List.filter_map
      (fun (ix, tags, _) ->
        match List.rev tags with
        | [] -> None
        | tag :: _ -> if Tag.is_value tag then Some ix else None)
      data
    |> Array.of_list
  in
  let max_size = Output.max_size output in
  { map; operators; values; max_size }

let shrink_int32 n =
  match n with
  | Some n ->
    let* v =
      Random.choose
        [ (1, Random.return 0l)
        ; (5, Random.return (Int32.div n 2l))
        ; (1, Random.return (Int32.sub n Int32.one))
        ]
    in
    Random.return @@ Some v
  | None -> Random.return None

let remove_operator t =
  if Array.length t.operators = 0 then
    Random.return t
  else
    let* ix = Random.range 0 @@ Array.length t.operators in
    let ix = Array.get t.operators ix in
    let map = IM.add ix None t.map in
    let operators =
      Array.to_list t.operators
      |> List.filteri (fun i _ -> i <> ix)
      |> Array.of_list
    in
    Random.return { t with map; operators }

let remove_value t =
  if Array.length t.values = 0 then
    Random.return t
  else
    let* ix = Random.range 0 @@ Array.length t.values in
    let ix = Array.get t.values ix in
    let map = IM.add ix None t.map in
    let values =
      Array.to_list t.values
      |> List.filteri (fun i _ -> i <> ix)
      |> Array.of_list
    in
    Random.return { t with map; values }

let shrink_operator t =
  if Array.length t.operators = 0 then
    Random.return t
  else
    let* arr_ix = Random.range 0 @@ Array.length t.operators in
    let ix = Array.get t.operators arr_ix in
    let* n = shrink_int32 @@ IM.find ix t.map in
    let map = IM.add ix n t.map in
    match n with
    | Some n when Int32.equal Int32.zero n ->
      let operators =
        Array.to_list t.operators
        |> List.filteri (fun i _ -> i <> arr_ix)
        |> Array.of_list
      in
      Random.return { t with map; operators }
    | _ -> Random.return { t with map }

let shrink_value t =
  if Array.length t.values = 0 then
    Random.return t
  else
    let* arr_ix = Random.range 0 @@ Array.length t.values in
    let ix = Array.get t.values arr_ix in
    let* n = shrink_int32 @@ IM.find ix t.map in
    let map = IM.add ix n t.map in
    match n with
    | Some n when Int32.equal Int32.zero n ->
      let values =
        Array.to_list t.values
        |> List.filteri (fun i _ -> i <> arr_ix)
        |> Array.of_list
      in
      Random.return { t with map; values }
    | _ -> Random.return { t with map }

let modify t =
  if Array.length t.values = 0 then
    shrink_operator t
  else if Array.length t.operators = 0 then
    shrink_value t
  else
    let modify t =
      Random.choose
        [ (5, shrink_value t)
        ; (2, shrink_operator t)
        ; (1, remove_operator t)
        ; (1, remove_value t)
        ]
    in
    let modify_twice t =
      let open Random.Syntax in
      let* t = modify t in
      modify t
    in
    Random.choose [ (5, modify t); (1, modify_twice t) ]

let to_input { map; max_size; _ } =
  let map = IM.filter_map (fun _ o -> o) map in
  Input.of_seq
    ~max_size
    (Seq.unfold
       (fun ix ->
         let v =
           match IM.find_opt ix map with
           | Some v -> v
           | None -> 0l
         in
         Some (v, ix + 1))
       0)

type stream = t

let shrink output (gen : Proposition.t Generator.t) =
  let open Random.Syntax in
  let module Config = struct
    type t = stream

    let compare { map = m1; _ } { map = m2; _ } =
      IM.compare (Option.compare Int32.compare) m1 m2

    let modify = modify
    let max_tries = 10_000

    let keep n =
      let output = Generator.run (to_input n) gen in
      match Output.value output with
      | Proposition.Fail _ -> true
      | _ -> false
  end
  in
  let module S = Search.Make (Config) in
  let* { Search.node; num_attempts; num_explored = num_shrinks } =
    S.search @@ of_output output
  in
  let input = to_input node in
  Random.return
    (let output = Generator.run input gen in
     match Output.value output with
     | Proposition.Fail { pp; _ } -> { num_shrinks; num_attempts; pp; output }
     | _ -> failwith "Should not return a non-failure")
