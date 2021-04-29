open Popper
open Sample.Syntax

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
    ~config:Config.(all [ verbose; max_size 400; num_samples 50 ])
    (fun () ->
      let* p = Sample.with_log "p" pp sample in
      equal comparator p p)

let suite = suite [ ("Equal itself", test) ]
