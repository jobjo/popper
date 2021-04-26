open Popper
open Syntax

let seed = Random.Seed.make 42

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
  let () = run_test test in
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
    [ lt Comparator.float 0.45 neg
    ; lt Comparator.float neg 0.5
    ; lt Comparator.float 0.45 nneg
    ; lt Comparator.float nneg 0.5
    ; lt Comparator.float zero 0.05
    ]

let int_range =
  let open Sample in
  test @@ fun () ->
  let* n1 = Int.range 0 10 in
  let* n2 = Int.range (-10) 10 in
  let* n3 = Int.range 0 0 in
  all
    [ eq Comparator.int 0 n3
    ; is_true (n1 >= 0)
    ; is_true (n1 < 10)
    ; is_true (n2 >= -10 && n2 < 10)
    ]

let int_negative =
  let open Sample in
  test @@ fun () ->
  let* n1 = Int.negative in
  lt Comparator.int n1 0

let int_positive =
  let open Sample in
  test @@ fun () ->
  let* n1 = Int.positive in
  gt Comparator.int n1 0

let float_classes =
  test @@ fun () ->
  let lookup = dist Float.classify_float Sample.float in
  let inf = lookup Float.FP_infinite in
  let zero = lookup Float.FP_zero in
  let normal = lookup Float.FP_normal in
  let nan = lookup Float.FP_nan in
  let* () = Sample.log_key_value "inf" (string_of_float inf) in
  let* () = Sample.log_key_value "normal" (string_of_float normal) in
  let* () = Sample.log_key_value "zero" (string_of_float zero) in
  let* () = Sample.log_key_value "nan" (string_of_float nan) in
  all
    [ lt Comparator.float 0. inf
    ; lt Comparator.float inf 0.02
    ; lt Comparator.float 0.95 normal
    ; lt Comparator.float 0.0 nan
    ; lt Comparator.float nan 0.1
    ]

let list_length =
  let open Sample in
  test @@ fun () ->
  let* n = Int.range 0 10 in
  let* xs = List.of_length n int in
  eq Comparator.int (Stdlib.List.length xs) n

let list_range =
  let open Sample in
  test @@ fun () ->
  let* n = Int.range 0 10 in
  let* xs = List.range 0 n int in
  lte Comparator.int (Stdlib.List.length xs) n

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
  let* s = with_log "s" Format.pp_print_string @@ Sample.String.upper in
  String.to_seq s |> List.of_seq |> List.for_all is_upper |> is_true

let string_alpha =
  test @@ fun () ->
  let* s = with_log "s" Format.pp_print_string @@ Sample.String.upper in
  String.to_seq s
  |> List.of_seq
  |> List.for_all (fun c -> is_upper c || is_lower c)
  |> is_true

let string_alpha_numeric =
  test @@ fun () ->
  let* s = with_log "s" Format.pp_print_string @@ Sample.String.alpha_numeric in
  String.to_seq s |> List.of_seq |> List.for_all is_alpha_numeric |> is_true

let string_num =
  test @@ fun () ->
  let* s = with_log "s" Format.pp_print_string @@ Sample.String.alpha_numeric in
  String.to_seq s |> List.of_seq |> List.for_all is_alpha_numeric |> is_true

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
    ]
