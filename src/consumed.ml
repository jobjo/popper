type t =
  | Entries of Int32.t list
  | Add of t * t
  | Tag of Tag.t * t

let add c1 c2 = Add (c1, c2)
let tag t c = Tag (t, c)
let make t ds = tag t @@ Entries ds
let empty = Entries []

let to_list t =
  let data = ref [] in
  let add t d = data := (t, d) :: !data in
  let rec aux t = function
    | Entries ds -> List.iter (add t) ds
    | Add (l, r) ->
      aux t l;
      aux t r
    | Tag (tag, t) -> aux tag t
  in
  aux Tag.Value t;
  List.rev !data
