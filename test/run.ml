open Popper
open Format

type person =
  { name : string
  ; age : int option
  ; friends : person list
  ; address : address option
  }

and address = { street : string } [@@deriving show, popper]

type contact =
  | Email of string
  | Address of string * int
[@@deriving show, popper]

let my_test =
  Test.test ~count:1 (fun () ->
    let open Generator.Syntax in
    let* person, consumed = Generator.with_consumed generate_person in
    fprintf
      std_formatter
      "@.@[<v>%a@,%a@]"
      Consumed.pp
      consumed
      pp_person
      person;
    Test.is_true true)

(* let () = Test.run my_test *)
