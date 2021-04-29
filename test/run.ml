let suite =
  Popper.suite
    [ ("Deriving Sample", Deriving_sample.suite)
    ; ("Deriving Popper", Deriving_popper.suite)
    ; ("Samples", Samples.suite)
    ]

let () = Popper.run suite
