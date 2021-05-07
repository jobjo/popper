(* open Popper

   let suite = suite [ ("Hello World", Hello_world.suite) ; ("Expression",
   Exp.suite) ; ("Chart", Chart.suite) ; ("Arithmetic", Arithmetic.suite) ;
   ("Tree", Tree.suite) ; ("Equal", Equal.suite) ]

   let () = run suite *)

open Popper
open Sample.Syntax

type contact =
  | Email of string
  | Mail of
      { street : string
      ; number : int option
      ; zip : string
      }
[@@deriving show, ord, popper]

let test () =
  let* contact, consumed =
    sample_contact
    |> Sample.resize 10
    |> Sample.with_log "contact" pp_contact
    |> Sample.with_consumed
  in
  Format.printf "%a@.%a@." Consumed.pp consumed pp_contact contact;
  match contact with
  | Email _ -> pass
  | Mail _ -> fail "Expected email"

let () = check test
