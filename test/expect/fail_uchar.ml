open Popper
open Sample.Syntax

let () =
  Clean.run
  @@ test (fun () ->
       let* _ =
         Sample.(with_log "uchar" (Comparator.pp Comparator.uchar) uchar)
       in
       fail "Failed")
