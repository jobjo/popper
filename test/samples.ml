open Popper
open Sample.Syntax

let seed = 45

let dist f gen =
  let map = Hashtbl.create 32 in
  let count = ref 0 in
  let test () =
    incr count;
    let* x = gen in
    let key = f x in
    let count =
      match Hashtbl.find_opt map key with
      | Some n -> n + 1
      | None -> 0
    in
    Hashtbl.add map key count;
    pass
  in
  let () = check test in
  fun key ->
    match Hashtbl.find_opt map key with
    | Some n -> float n /. float !count
    | None -> 0.

let neg_pos_ratio =
  test @@ fun () ->
  let lookup =
    dist
      (fun n -> if n < 0 then `Neg else if n = 0 then `Zero else `Non_neg)
      Sample.int
  in
  let neg = lookup `Neg in
  let zero = lookup `Zero in
  let nneg = lookup `Non_neg in
  let* () = Sample.log_key_value "Negative" (string_of_float neg) in
  let* () = Sample.log_key_value "Non-negative" (string_of_float nneg) in
  let* () = Sample.log_key_value "Zero" (string_of_float zero) in
  all
    [ less_than Comparator.float 0.4 neg
    ; less_than Comparator.float neg 0.5
    ; less_than Comparator.float 0.5 nneg
    ; less_than Comparator.float nneg 0.6
    ; less_than Comparator.float zero 0.05
    ]

let int_range =
  let open Sample in
  test @@ fun () ->
  let* n1 = Int.range 0 10 in
  let* n2 = Int.range (-10) 10 in
  let* n3 = Int.range 0 0 in
  all
    [ equal Comparator.int 0 n3
    ; is_true (n1 >= 0)
    ; is_true (n1 < 10)
    ; is_true (n2 >= -10 && n2 < 10)
    ]

let int_negative =
  let open Sample in
  test @@ fun () ->
  let* n1 = Int.negative in
  less_than Comparator.int n1 0

let int_positive =
  let open Sample in
  test @@ fun () ->
  let* n1 = Int.positive in
  greater_than Comparator.int n1 0

let float_classes =
  test @@ fun () ->
  let lookup = dist Float.classify_float Sample.float in
  let inf = lookup Float.FP_infinite in
  let zero = lookup Float.FP_zero in
  let normal = lookup Float.FP_normal in
  let nan = lookup Float.FP_nan in
  let sub_normal = lookup Float.FP_subnormal in
  let* () = Sample.log_key_value "inf" (string_of_float inf) in
  let* () = Sample.log_key_value "normal" (string_of_float normal) in
  let* () = Sample.log_key_value "zero" (string_of_float zero) in
  let* () = Sample.log_key_value "nan" (string_of_float nan) in
  let* () = Sample.log_key_value "sub_normal" (string_of_float sub_normal) in
  all
    [ less_than Comparator.float 0. inf
    ; less_than Comparator.float inf 0.05
    ; less_than Comparator.float 0.9 normal
    ; less_than Comparator.float 0.0 nan
    ; less_than Comparator.float nan 0.1
    ]

let list_length =
  let open Sample in
  test @@ fun () ->
  let* n = Int.range 0 10 in
  let* xs = List.of_length n int in
  equal Comparator.int (Stdlib.List.length xs) n

let list_range =
  let open Sample in
  test @@ fun () ->
  let* n = Int.range 0 10 in
  let* xs = List.range 0 n int in
  less_equal_than Comparator.int (Stdlib.List.length xs) n

let is_lower c = 'a' <= c && c <= 'z'
let is_upper c = 'A' <= c && c <= 'Z'
let is_num c = '0' <= c && c <= '9'
let is_alpha_numeric c = is_lower c || is_upper c || is_num c

let string_lower =
  test @@ fun () ->
  let* s = Sample.String.lower in
  String.to_seq s
  |> List.of_seq
  |> List.for_all (fun c -> 'a' <= c && c <= 'z')
  |> is_true

let string_upper =
  test @@ fun () ->
  let* s = Sample.with_log "s" Format.pp_print_string @@ Sample.String.upper in
  String.to_seq s |> List.of_seq |> List.for_all is_upper |> is_true

let string_alpha =
  test @@ fun () ->
  let* s = Sample.with_log "s" Format.pp_print_string @@ Sample.String.upper in
  String.to_seq s
  |> List.of_seq
  |> List.for_all (fun c -> is_upper c || is_lower c)
  |> is_true

let string_alpha_numeric =
  test @@ fun () ->
  let* s =
    Sample.with_log "s" Format.pp_print_string @@ Sample.String.alpha_numeric
  in
  String.to_seq s |> List.of_seq |> List.for_all is_alpha_numeric |> is_true

let string_num =
  test @@ fun () ->
  let* s =
    Sample.with_log "s" Format.pp_print_string @@ Sample.String.alpha_numeric
  in
  String.to_seq s |> List.of_seq |> List.for_all is_alpha_numeric |> is_true

type t1 =
  { xs : int list
  ; ys : int list
  ; zs : string list
  }
[@@deriving ord, show, popper]

type t2 = int list * int list * string list [@@deriving ord, show, popper]

let gen_dist_test get_xs get_ys get_zs gen =
  let num_xs = ref 0 in
  let num_ys = ref 0 in
  let num_zs = ref 0 in
  let accum t =
    num_xs := !num_xs + (List.length @@ get_xs t);
    num_ys := !num_ys + (List.length @@ get_ys t);
    num_zs := !num_zs + (List.length @@ get_zs t)
  in
  test @@ fun () ->
  let _ = dist accum gen () in
  let* () = Sample.log_key_value "num-xs" (string_of_int !num_xs) in
  let* () = Sample.log_key_value "num-ys" (string_of_int !num_ys) in
  let* () = Sample.log_key_value "num-zs" (string_of_int !num_zs) in
  let avg = (!num_xs + !num_ys + !num_zs) / 3 in
  let diff_xs = float (abs (!num_xs - avg)) /. float avg in
  let diff_ys = float (abs (!num_ys - avg)) /. float avg in
  let diff_zs = float (abs (!num_zs - avg)) /. float avg in
  let* () = Sample.log_key_value "diff_xs" (string_of_float diff_xs) in
  let* () = Sample.log_key_value "diff_ys" (string_of_float diff_ys) in
  let* () = Sample.log_key_value "diff_zs" (string_of_float diff_zs) in
  all
    [ less_than Comparator.float diff_xs 0.05
    ; less_than Comparator.float diff_ys 0.05
    ; less_than Comparator.float diff_zs 0.05
    ]

let tuple_with_list_length_dist =
  gen_dist_test
    (fun (xs, _, _) -> xs)
    (fun (_, ys, _) -> ys)
    (fun (_, _, zs) -> zs)
    (Sample.Tuple.tripple
       (Sample.list Sample.int)
       (Sample.list Sample.int)
       (Sample.list Sample.string))

let derived_tuple_with_list_length_dist =
  gen_dist_test
    (fun (xs, _, _) -> xs)
    (fun (_, ys, _) -> ys)
    (fun (_, _, zs) -> zs)
    t2_sample

let record_with_list_length_dist =
  gen_dist_test
    (fun { xs; _ } -> xs)
    (fun { ys; _ } -> ys)
    (fun { zs; _ } -> zs)
    t1_sample

let manual_with_list_length_dist =
  let sample =
    let* xs = Sample.(list int) in
    let* ys = Sample.(list int) in
    let* zs = Sample.(list int) in
    Sample.return (xs, ys, zs)
  in
  gen_dist_test
    (fun (xs, _, _) -> xs)
    (fun (_, ys, _) -> ys)
    (fun (_, _, zs) -> zs)
    sample

let suite =
  suite
    [ ("Int range", int_range)
    ; ("Int positive", int_positive)
    ; ("Int negative", int_negative)
    ; ("Float classes", float_classes)
    ; ("Neg/pos ratio", neg_pos_ratio)
    ; ("List length", list_length)
    ; ("List range", list_range)
    ; ("String lower", string_lower)
    ; ("String upper", string_upper)
    ; ("String alpha", string_alpha)
    ; ("String num", string_num)
    ; ("String alpha-numeric", string_alpha_numeric)
    ; ("Record list length dist", record_with_list_length_dist)
    ; ("Derived list length dist", derived_tuple_with_list_length_dist)
    ; ("Tuple list length dist", tuple_with_list_length_dist)
    ; ("Manual list length dist", manual_with_list_length_dist)
    ]
