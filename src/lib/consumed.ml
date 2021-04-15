type t =
  | Value of Int32.t
  | Empty
  | Add of t * t
  | Tag of Tag.t * t

let add c1 c2 =
  match (c1, c2) with
  | Empty, c2 -> c2
  | c1, Empty -> c1
  | _ -> Add (c1, c2)

let tag t c = Tag (t, c)
let value d = Value d
let empty = Empty

let rec is_empty = function
  | Value _ -> false
  | Add (l, r) -> is_empty l && is_empty r
  | Tag (_, c) -> is_empty c
  | Empty -> true

let to_list t =
  let data = ref [] in
  let tag_count = ref 0 in
  let add t d = data := (t, d) :: !data in
  let rec aux tags = function
    | Value ds -> add tags ds
    | Add (l, r) ->
      aux tags l;
      aux tags r
    | Empty -> ()
    | Tag (tag, t) ->
      incr tag_count;
      let tags = (tag, !tag_count) :: tags in
      aux tags t
  in
  aux [] t;
  List.rev !data |> List.map (fun (ts, v) -> (List.rev ts, v))

let show_tag (tag, ix) = Printf.sprintf "%s-%d" (Tag.to_string tag) ix

let pp out t =
  let open Format in
  List.mapi
    (fun ix (tags, data) ->
      [ Table.cell (fun out () -> fprintf out "%d" ix)
      ; Table.cell (fun out () ->
          fprintf out "%s" @@ String.concat "," @@ List.map show_tag tags)
      ; Table.cell (fun out () -> fprintf out "%d" @@ Int32.to_int data)
      ])
    (to_list t)
  |> Table.of_list ~columns:[ Table.right; Table.left; Table.right ]
  |> Table.pp out
