let suite =
  Popper.suite
    [ ("Deriving Sample", Deriving_sample.suite)
    ; ("Deriving Popper", Deriving_popper.suite)
    ; ("Samples", Samples.suite)
    ; ("Config", Config.suite)
    ]

type foo = { bar : int array } [@@deriving show, ord, popper]

let config = Popper.Config.num_samples 1000
let () = Popper.run ~config suite
