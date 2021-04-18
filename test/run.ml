open Popper

type person =
  { name : string
  ; age : int
  }
[@@deriving show, ord, popper]

let suite =
  suite
    [ ("Deriving Generator", Deriving_generator.suite)
    ; ("Deriving Popper", Deriving_popper.suite)
    ; ("Generator", Generators.suite)
    ]

let () = run suite
