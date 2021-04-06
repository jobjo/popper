open Popper
open Generator.Syntax

type t =
  | Lit of bool
  | And of t * t
  | Or of t * t
  | Not of t
[@@deriving show, popper]

let rec eval = function
  | Lit b -> b
  | And (e1, e2) -> eval e1 || eval e2
  | Or (e1, e2) -> eval e1 || eval e2
  | Not b -> not @@ eval b

let test_and =
  let open Generator in
  Test.test (fun () ->
    let* e1 = generate in
    let* e2 = generate in
    let* () = log_key_value "e1" (show e1) in
    let* () = log_key_value "e2" (show e2) in
    let condition = (eval e1 && eval e2) = eval (And (e1, e2)) in
    Test.is_true ~loc:__LOC__ condition)

let test_or =
  Test.test (fun () ->
    let* e1 = generate in
    let* e2 = generate in
    Test.is_true ((eval e1 || eval e2) = eval (Or (e1, e2))))

let suite = Test.suite [ "Exp and", test_and; "Exp or", test_or ]
