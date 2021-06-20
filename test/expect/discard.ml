open Popper

let test_discarded = test (fun () -> discard)
let () = Clean.run test_discarded
