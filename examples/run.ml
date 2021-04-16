open Popper

let suite =
  Test.suite
    [ ("Hello World", Hello_world.suite)
    ; ("Expression", Exp.suite)
    ; ("Chart", Chart.suite)
    ; ("Arithmetic", Arithmetic.suite)
    ; ("Tree", Tree.suite)
    ; ("Equal", Equal.suite)
    ]

(* let () = Test.run suite *)
