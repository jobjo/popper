type t =
  | Entries of Int32.t list
  | Add of t * t
  | Tag of Tag.t * t

let add c1 c2 = Add (c1, c2)
let tag t c = Tag (t, c)
let make ds = Entries ds
let empty = Entries []

let rec is_empty = function
  | Entries [] -> true
  | Add (l, r) -> is_empty l && is_empty r
  | Tag (_, c) -> is_empty c
  | _ -> false

let to_list t =
  let data = ref [] in
  let add t d = data := (t, d) :: !data in
  let rec aux tags = function
    | Entries ds -> List.iter (add tags) ds
    | Add (l, r) ->
      aux tags l;
      aux tags r
    | Tag (tag, t) ->
      let tags = tag :: tags in
      aux tags t
  in
  aux [] t;
  List.rev !data |> List.map (fun (ts, v) -> (List.rev ts, v))

let pp out t =
  let open Format in
  List.mapi
    (fun ix (tags, data) ->
      [ Table.cell (fun out () -> fprintf out "%d" ix)
      ; Table.cell (fun out () ->
          fprintf out "%s" @@ String.concat "," @@ List.map Tag.to_string tags)
      ; Table.cell (fun out () -> fprintf out "%d" @@ Int32.to_int data)
      ])
    (to_list t)
  |> Table.of_list ~columns:[ Table.right; Table.left; Table.right ]
  |> Table.pp out
