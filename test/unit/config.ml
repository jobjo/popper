open Popper
open Sample.Syntax

let skip_output = Config.formatter Format.str_formatter

let test_exceed_num_discarded =
  test @@ fun () ->
  try
    let config = Config.(all [ max_num_discarded 50; skip_output ]) in
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
  let config = Config.(max_num_discarded 1100) in
  test ~config @@ fun () ->
  let* b = Sample.bool in
  if b then
    pass
  else
    discard

let test_discard_unit =
  test @@ fun () ->
  try
    (* This test should run without throwing *)
    check ~config:skip_output (fun () -> discard);
    pass
  with
  | _ -> fail "Did not expect discard to throw"

let suite =
  suite
    [ ("Max num discarded", test_exceed_num_discarded)
    ; ("Some discarded", test_some_discarded)
    ; ("Discard unit", test_discard_unit)
    ]
