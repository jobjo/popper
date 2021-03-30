open Popper

let suite =
  Test.suite [ "Hello World", Hello_world.suite; "Expression", Exp.suite ]

let () = Test.run suite
