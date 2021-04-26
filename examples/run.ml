open Popper

let suite =
  suite
    [ ("Hello World", Hello_world.suite)
    ; ("Expression", Exp.suite)
    ; ("Chart", Chart.suite)
    ; ("Arithmetic", Arithmetic.suite)
    ; ("Tree", Tree.suite)
    ; ("Equal", Equal.suite)
    ]

let () = run ~config:(Config.num_samples 10) suite
