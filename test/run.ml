open Popper

let suite =
  Test.suite
    [ ("Deriving Generator", Deriving_generator.suite)
    ; ("Deriving Popper", Deriving_popper.suite)
    ]

let () = Popper.Test.run suite
