open Popper
open Sample.Syntax

let test_exceed_num_discarded =
  test @@ fun () ->
  try
    let config = Config.max_num_discarded 50 in
    check ~config (fun () ->
      let* b = Sample.bool in
      if b then
        pass
      else
        discard);
    fail "Should have thrown an exception"
  with
  | _ -> pass

let test_some_discarded =
  test @@ fun () ->
  let* b = Sample.bool in
  if b then
    pass
  else
    discard

let test_discard_unit =
  test @@ fun () ->
  try
    (* This test should run without throwing *)
    check (fun () -> discard);
    pass
  with
  | _ -> fail "Did not expect discard to throw"

let suite =
  suite
    [ ("Max num discarded", test_exceed_num_discarded)
    ; ("Some discarded", test_some_discarded)
    ; ("Discard unit", test_discard_unit)
    ]
