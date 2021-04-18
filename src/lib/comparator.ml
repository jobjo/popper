open Format

type 'a t =
  { compare : 'a -> 'a -> int
  ; pp : Format.formatter -> 'a -> unit
  }

let make compare pp = { compare; pp }
let compare { compare; _ } = compare
let eq { compare; _ } x y = compare x y = 0
let pp { pp; _ } out t = pp out t

let pp_tuple pp1 pp2 out (x, y) =
  fprintf out "[@<hv 2>(%a@,,@,%a)@]" pp1 x pp2 y

let pp_list pp out xs =
  fprintf
    out
    "@[<hv 2>[@,%a]@]"
    (pp_print_list ~pp_sep:(fun out () -> pp_print_string out ",") pp)
    xs

let tuple t1 t2 =
  make
    (fun (x1, y1) (x2, y2) ->
      let c1 = compare t1 x1 x2 in
      if c1 < 0 then
        -1
      else if c1 = 0 then
        compare t2 y1 y2
      else
        1)
    (pp_tuple (pp t1) (pp t2))

let list t =
  let rec comp_list xs ys =
    match (xs, ys) with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x :: xs, y :: ys ->
      let r = compare t x y in
      if r = 0 then
        comp_list xs ys
      else
        r
  in
  make comp_list (pp_list @@ pp t)

let int = make Int.compare pp_print_int
let float = make Float.compare pp_print_float
let string = make String.compare pp_print_string
let bool = make Bool.compare pp_print_bool

let option t =
  let compare = compare t in
  let printer =
    pp_print_option ~none:(fun formatter () -> fprintf formatter "None") @@ pp t
  in
  make (Option.compare compare) printer
