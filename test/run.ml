open Popper

let suite = Test.suite [ ("Deriving", Deriving.suite) ]
let () = Popper.Test.run suite
