open Popper

let suite = Test.suite [ "Expression", Exp.suite; "Chart", Chart.suite ]
let () = Test.run suite
