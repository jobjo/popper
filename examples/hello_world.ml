open Popper
open Generator
open Syntax

let test_rev =
  Test.test (fun () ->
    Test.equal
      ~loc:__LOC__
      Comparator.(list int)
      (List.rev [ 1; 2; 3 ])
      [ 2; 3; 1 ])

let test_rev_twice =
  Test.test (fun () ->
    let* xs = list int in
    Test.equal ~loc:__LOC__ Comparator.(list int) (List.rev (List.rev xs)) xs)

let suite =
  Test.suite [ ("Reverse", test_rev); ("Reverse twice", test_rev_twice) ]
