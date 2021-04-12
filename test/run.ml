open Popper

let suite =
  suite
    [ ("Deriving Generator", Deriving_generator.suite)
    ; ("Deriving Popper", Deriving_popper.suite)
    ]

(* let () = run suite *)
