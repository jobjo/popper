open Popper
open Syntax

type tag =
  [ `Foo
  | `Bar of float * bool
  ]
[@@deriving show, ord, popper]

type t =
  { name : string
  ; tags : tag list
  }
[@@deriving show, ord, popper]

let test =
  Popper.test
    ~configs:[ Config.verbose; Config.max_size 400; Config.num_samples 50 ]
    (fun () ->
    let* p = with_log "p" pp sample in
    eq comparator p p)

let suite = suite [ ("Equal itself", test) ]
