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
  Test.test ~count:200 (fun () ->
    let open Generator.Syntax in
    let* person, consumed = Generator.with_consumed generate_person in
    let f () =
      fprintf
        std_formatter
        "@.@.@[<v 2>Consumed:@,%a@]@;@;@[<v 2>Person:@,%a@]"
        Consumed.pp
        consumed
        pp_person
        person
    in
    let _ = f in
    Test.is_true (List.length person.friends <> 10))

(* let () = Test.run my_test *)
