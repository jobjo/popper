module R = Random
open Random.Syntax
module IM = Map.Make (Int)
module IS = Set.Make (Int)

type result =
  { num_shrinks : int
  ; num_attempts : int
  ; pp : Format.formatter -> unit -> unit
  ; output : Proposition.t Output.t
  }

type indexed =
  | Value of int * Int32.t
  | Empty
  | Add of int * int * indexed * indexed
  | Tag of int * int * Tag.t * indexed

type t =
  { indexed : indexed
  ; nodes : indexed IM.t
  ; overrides : Consumed.t IM.t
  ; node_indexes : int array
  }

let zero = Consumed.value 0l

let indexed_of_consumed node =
  let rec aux ix = function
    | Consumed.Value v -> (Value (ix, v), ix)
    | Empty -> (Empty, ix)
    | Add (l, r) ->
      let l, ix1 = aux (ix + 1) l in
      let r, ix2 = aux (ix1 + 1) r in
      (Add (ix, ix2, l, r), ix2)
    | Tag (t, c) ->
      let c, ix2 = aux (ix + 1) c in
      (Tag (ix, ix2, t, c), ix2)
  in
  fst @@ aux 0 node

let pp out { indexed; nodes; overrides; node_indexes } =
  let open Format in
  let rec pp_indexed out indexed =
    let open Format in
    match indexed with
    | Value (ix, v) -> fprintf out "@[- Value[%d] %ld@]" ix v
    | Empty -> fprintf out "[@- Empty@]"
    | Tag (ix1, ix2, tg, t) ->
      fprintf
        out
        "@[<v 2>- Tag.%s[%d,%d]@,%a@]"
        (Tag.to_string tg)
        ix1
        ix2
        pp_indexed
        t
    | Add (ix1, ix2, l, r) ->
      fprintf
        out
        "@[<v 2>- Add[%d,%d]@,%a@,%a@]"
        ix1
        ix2
        pp_indexed
        l
        pp_indexed
        r
  in
  let pp_nodes out nodes =
    let pp out () =
      IM.iter
        (fun ix indexed ->
          fprintf out "@[<v>%d: @;%a@,@]" ix pp_indexed indexed)
        nodes
    in
    fprintf out "@[<v 2>Nodes:@,%a@]" pp ()
  in
  let pp_overrides out overrides =
    let pp out () =
      IM.iter
        (fun ix c ->
          let indexed = indexed_of_consumed c in
          fprintf out "@[<v 2>Index: %d@,%a@]" ix pp_indexed indexed)
        overrides
    in
    fprintf out "@[<v>Overrides:@,%a@]" pp ()
  in
  let pp_indexes out node_indexes =
    fprintf
      out
      "@[<v 2>Node indexes:@,@[<hv>@,[%a]@]@]"
      (pp_print_list (fun o d -> fprintf o "%d; " d))
      (Array.to_list node_indexes)
  in
  fprintf
    out
    "@[<v 2>Shrink-structure:@,%a@,%a@,%a@,%a@]"
    pp_indexed
    indexed
    pp_nodes
    nodes
    pp_overrides
    overrides
    pp_indexes
    node_indexes

let rec unindex = function
  | Empty -> Consumed.empty
  | Add (_, _, l, r) -> Consumed.add (unindex l) (unindex r)
  | Tag (_, _, t, c) -> Consumed.tag t @@ unindex c
  | Value (_, v) -> Consumed.value v

let of_consumed consumed =
  let map = ref IM.empty in
  let add_node ix1 node = map := IM.add ix1 node !map in
  let rec aux n =
    match n with
    | Empty -> ()
    | Value (ix, _) -> add_node ix n
    | Add (ix1, _, l, r) ->
      add_node ix1 n;
      aux l;
      aux r
    | Tag (ix1, _, _, c) ->
      add_node ix1 n;
      aux c
  in
  let indexed = indexed_of_consumed consumed in
  aux indexed;
  let nodes = !map in
  let overrides = IM.empty in
  let node_indexes = IM.to_seq nodes |> Array.of_seq |> Array.map fst in
  { indexed; nodes; overrides; node_indexes }

let to_consumed { indexed; overrides; _ } =
  let rec aux = function
    | Empty -> Consumed.empty
    | Add (ix, _, l, r) ->
      (match IM.find_opt ix overrides with
      | Some n -> n
      | None -> Consumed.add (aux l) (aux r))
    | Tag (ix, _, t, c) ->
      (match IM.find_opt ix overrides with
      | Some n -> n
      | None -> Consumed.tag t @@ aux c)
    | Value (ix, v) ->
      (match IM.find_opt ix overrides with
      | Some n -> n
      | None -> Consumed.value v)
  in
  aux indexed

let shrink_value n =
  Random.choose_value
    [ (10., zero)
    ; (10., Consumed.value @@ Int32.div n 2l)
    ; (1., Consumed.value @@ Int32.sub n Int32.one)
    ]

let shrink_node c =
  match c with
  | Consumed.Tag (Tag.Sub_list, Tag (Tag.Choice, Value 0l)) -> R.return None
  | Tag (Tag.Sub_list, _) -> R.return (Some zero)
  | Tag (Tag.List, _) -> R.return (Some zero)
  | Tag (Tag.Bool, _) -> R.return (Some zero)
  | Add (l, r) ->
    R.choose_value [ (2., Some zero); (1., Some (Consumed.add r l)) ]
  | Value v ->
    let+ v = shrink_value v in
    Some v
  | _ -> R.return None

let try_shrink { indexed; nodes; overrides; node_indexes } =
  let* arr_ix = R.range 0 @@ Array.length node_indexes in
  let node_ix = Array.get node_indexes arr_ix in
  let+ override = shrink_node @@ unindex @@ IM.find node_ix nodes in
  match override with
  | Some override ->
    let overrides = IM.add node_ix override overrides in
    let t = { indexed; nodes; overrides; node_indexes } in
    Some t
  | None -> None

let modify data =
  let open Random.Syntax in
  let+ r =
    Random.until_some
      ~max_tries:100
      (Random.delayed @@ fun () -> try_shrink data)
  in
  match r with
  | Some x -> x
  | None -> data

let to_input ~size c =
  c |> to_consumed |> Consumed.to_list |> List.map snd |> Input.of_list ~size

let shrink output (gen : Proposition.t Sample.t) =
  let size = Output.size output in
  let node = of_consumed @@ Output.consumed output in
  let keep t =
    let input = to_input ~size t in
    let output = Sample.run input gen in
    match Output.value output with
    | Proposition.Fail _ -> Some (of_consumed @@ Output.consumed output)
    | _ -> None
  in
  let module Config = struct
    type nonrec t = t

    let max_tries = 100

    let compare t1 t2 =
      compare
        (List.map snd @@ Consumed.to_list @@ to_consumed t1)
        (List.map snd @@ Consumed.to_list @@ to_consumed t2)

    let modify t = modify t
    let keep = keep
  end
  in
  let module S = Search.Make (Config) in
  let* { Search.num_attempts; num_explored = num_shrinks; node } =
    let f { Search.node; _ } =
      node |> to_consumed |> Consumed.to_list |> List.length
    in
    R.best_of ~num_tries:10 f (S.search node)
  in
  let input = to_input ~size node in
  let output = Sample.run input gen in
  Random.return
    (match Output.value output with
    | Proposition.Fail { pp; _ } -> { num_shrinks; num_attempts; pp; output }
    | _ -> failwith "Should not return a non-failure")
