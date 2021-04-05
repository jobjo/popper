open Format

type 'a t =
  { equal : 'a -> 'a -> bool
  ; pp : Format.formatter -> 'a -> unit
  }

let make equal pp = { equal; pp }
let equal { equal; _ } = equal
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
    (fun (x1, y1) (x2, y2) -> equal t1 x1 x2 && equal t2 y1 y2)
    (pp_tuple (pp t1) (pp t2))

let list t =
  make
    (fun xs ys ->
      if List.length xs <> List.length ys then
        false
      else
        List.for_all (fun (x, y) -> equal t x y) @@ List.combine xs ys)
    (pp_list @@ pp t)

let int = make ( = ) pp_print_int
let string = make String.equal pp_print_string
let bool = make Bool.equal pp_print_bool

let option t =
  let eq = equal t in
  let printer =
    pp_print_option ~none:(fun formatter () -> fprintf formatter "None") @@ pp t
  in
  make
    (fun x_opt y_opt ->
      match x_opt, y_opt with
      | Some x, Some y -> eq x y
      | None, None -> true
      | _, _ -> false)
    printer
