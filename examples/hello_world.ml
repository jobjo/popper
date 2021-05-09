open Popper
open Sample.Syntax

type int_list = int list [@@deriving show, ord, popper]

let eq_list = equal int_list_comparator
let test = test ~config:(Config.num_samples 1000)

let test_rev =
  test (fun () ->
    all
      [ eq_list (List.rev []) []
      ; eq_list (List.rev [ 1 ]) [ 1 ]
      ; eq_list (List.rev [ 1; 2; 3 ]) [ 2; 3; 1 ]
      ])

let test_rev_twice =
  test (fun () ->
    let* xs = int_list_sample in
    eq_list (List.rev (List.rev xs)) xs)

let suite = suite [ ("Reverse", test_rev); ("Reverse twice", test_rev_twice) ]
