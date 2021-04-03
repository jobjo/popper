type t =
  | Entries of Int32.t list
  | Add of t * t
  | Tag of Tag.t * t

let add c1 c2 = Add (c1, c2)
let tag t c = Tag (t, c)
let make ds = Entries ds
let empty = Entries []

let is_empty = function
  | Entries [] -> true
  | _ -> false

let to_list t =
  let data = ref [] in
  let add t d = data := (t, d) :: !data in
  let rec aux tag = function
    | Entries ds -> List.iter (add tag) ds
    | Add (l, r) ->
      aux tag l;
      aux tag r
    | Tag (tag2, t) ->
      let tag =
        match tag with
        | Some t -> t
        | None -> tag2
      in
      aux (Some tag) t
  in
  aux None t;
  List.rev !data
  |> List.map (fun (t, v) -> Option.fold ~none:Tag.Value ~some:Fun.id t, v)

let pp out t =
  let open Format in
  List.mapi
    (fun ix (tag, data) ->
      [ Table.cell (fun out () -> fprintf out "%d" ix)
      ; Table.cell (fun out () -> fprintf out "%s" @@ Tag.to_string tag)
      ; Table.cell (fun out () -> fprintf out "%d" @@ Int32.to_int data)
      ])
    (to_list t)
  |> Table.of_list ~columns:[ Table.right; Table.left; Table.right ]
  |> Table.pp out
