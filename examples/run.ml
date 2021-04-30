open Popper
open Sample.Syntax

type exp =
  | Lit of bool
  | And of exp * exp
  | Or of exp * exp
  | Not of exp
[@@deriving show, ord, popper]

let rec eval = function
  | Lit b -> b
  | And (e1, e2) -> eval e1 || eval e2
  | Or (e1, e2) -> eval e1 || eval e2
  | Not b -> not @@ eval b

(* A simple unit test *)
let test_hello_world =
  test @@ fun () ->
  equal Comparator.string "hello world" (String.lowercase_ascii "Hello World")

(* Another unit test*)
let test_lit_true = test @@ fun () -> is_true (eval (Lit true) = true)

(* A property-based test *)
let test_false_ident_or =
  test @@ fun () ->
  let* e = sample_exp in
  is_true (eval e = eval (Or (Lit false, e)))

let test_true_ident_and =
  test @@ fun () ->
  let* e = Sample.with_log "e" pp_exp sample_exp in
  is_true ~loc:__LOC__ (eval e = eval (And (Lit true, e)))

let suite =
  suite
    [ ("Hello World", test_hello_world)
    ; ("Lit true", test_lit_true)
    ; ("False ident or", test_false_ident_or)
    ; ("True ident and", test_true_ident_and)
    ]

let () = run suite
