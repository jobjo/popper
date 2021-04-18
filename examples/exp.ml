open Popper
open Syntax

type t =
  | Lit of bool
  | And of t * t
  | Or of t * t
  | Not of t
[@@deriving show, ord, popper]

let rec eval = function
  | Lit b -> b
  | And (e1, e2) -> eval e1 || eval e2
  | Or (e1, e2) -> eval e1 || eval e2
  | Not b -> not @@ eval b

let test_and =
  let open Generator in
  test ~verbose:() (fun () ->
    let* e1 = with_log "e1" pp generate in
    let* e2 = with_log "e2" pp generate in
    let condition = (eval e1 && eval e2) = eval (And (e1, e2)) in
    is_true ~loc:__LOC__ condition)

let test_or =
  test (fun () ->
    let* e1 = with_log "e1" pp generate
    and* e2 = with_log "e2" pp generate in
    eq Comparator.bool (eval e1 || eval e2) (eval (Or (e1, e2))))

let suite = Test.suite [ ("Exp and", test_and); ("Exp or", test_or) ]
