open Popper
open Sample.Syntax

let eq_list = equal Comparator.(list int)

let test_rev =
  test (fun () ->
    all
      [ eq_list (List.rev []) []
      ; eq_list (List.rev [ 1 ]) [ 1 ]
      ; eq_list (List.rev [ 1; 2; 3 ]) [ 2; 3; 1 ]
      ])

let test_rev_twice =
  test (fun () ->
    let* xs = Sample.list Sample.int in
    eq_list (List.rev (List.rev xs)) xs)

let suite = suite [ ("Reverse", test_rev); ("Reverse twice", test_rev_twice) ]
