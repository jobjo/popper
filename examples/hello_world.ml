open Popper
open Generator
open Syntax

let suite =
  Test.test
    (let* xs = many int in
     Test.is_true (List.rev @@ List.rev xs = xs))
