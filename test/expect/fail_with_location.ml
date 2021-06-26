open Popper

type int_list = int list [@@deriving show, ord, popper]

let test_rev =
  test @@ fun () ->
  equal ~loc:__LOC__ int_list_comparator (List.rev [ 1; 2; 3 ]) [ 2; 3; 1 ]

let () = Clean.run test_rev
