open Popper
open Syntax

type foo =
  [ `Foo
  | `Bar of float
  ]
[@@deriving show, ord, popper]

type person =
  { name : string
  ; age : foo option list
  }
[@@deriving show, ord, popper]

let test =
  Popper.test (fun () ->
    let* p = generate_person in
    eq person_comparator p p)

let suite = suite [ ("Person json", test) ]
