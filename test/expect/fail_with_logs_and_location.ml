open Popper
open Sample.Syntax

type int_list = int list [@@deriving show, ord, popper]

let test_rev =
  test @@ fun () ->
  let* () = Sample.log_key_value "x" "42" in
  let* () = Sample.log_key_value "y" "Dummy" in
  equal ~loc:__LOC__ int_list_comparator (List.rev [ 1; 2; 3 ]) [ 2; 3; 1 ]

let () = Clean.run test_rev
