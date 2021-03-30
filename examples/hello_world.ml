open Popper
open Generator
open Syntax

let test_rev =
  Test.unit (fun () ->
    Proposition.equal
      ~loc:__LOC__
      Comparable.(list int)
      (List.rev [ 1; 2; 3 ])
      [ 2; 3; 1 ])

let test_rev_twice =
  Test.test
    (let* xs = many int in
     Test.equal ~loc:__LOC__ Comparable.(list int) (List.rev (List.rev xs)) xs)

let suite = Test.suite [ "Reverse", test_rev; "Reverse twice", test_rev_twice ]
