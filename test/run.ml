open Popper

type person =
  { name : string
  ; age : int
  }
[@@deriving show, ord, popper]

let suite =
  suite
    [ ("Deriving Sample", Deriving_sample.suite)
    ; ("Deriving Popper", Deriving_popper.suite)
    ; ("Sample", Samples.suite)
    ]

let () = run suite
