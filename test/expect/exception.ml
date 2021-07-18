open Popper
open Sample.Syntax

let test_e1 = test @@ fun () -> assert false

let test_e2 =
  test @@ fun () ->
  let* () = Sample.return () in
  assert false

let test_e3 =
  test @@ fun () ->
  let* _ = Sample.int in
  assert false

let test_e4 =
  test @@ fun () ->
  let* n = Sample.int in
  if n mod 2 = 0 then
    fail "Not even"
  else
    assert false

let suite =
  suite
    [ ("Exception 1", test_e1)
    ; ("Exception 2", test_e2)
    ; ("Exception 3", test_e3)
    ; ("Exception 4", test_e4)
    ]

let () = Clean.run suite
