open Popper
open Syntax

let seed = Random.Seed.make_self_init ()

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
    let n = Hashtbl.find map key in
    float n /. float !count

let neg_pos_ratio =
  test ~verbose:() @@ fun () ->
  let lookup =
    dist
      (fun n -> if n < 0 then `Neg else if n = 0 then `Zero else `Non_neg)
      Generator.int
  in
  let neg = lookup `Neg in
  let zero = lookup `Zero in
  let nneg = lookup `Non_neg in
  let* () = Generator.log_key_value "Negative" (string_of_float neg) in
  let* () = Generator.log_key_value "Non-negative" (string_of_float nneg) in
  let* () = Generator.log_key_value "Zero" (string_of_float zero) in
  all
    [ lt Comparator.float 0.45 neg
    ; lt Comparator.float neg 0.5
    ; lt Comparator.float 0.45 nneg
    ; lt Comparator.float nneg 0.5
    ; lt Comparator.float zero 0.05
    ]

let int_range =
  let open Generator in
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

let suite = suite [ ("Int range", int_range); ("Neg/pos ratio", neg_pos_ratio) ]
