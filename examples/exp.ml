open Popper
open Sample.Syntax

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
  let open Sample in
  test ~config:Config.verbose (fun () ->
    let* e1 = with_log "e1" pp sample in
    let* e2 = with_log "e2" pp sample in
    let condition = (eval e1 && eval e2) = eval (And (e1, e2)) in
    is_true ~loc:__LOC__ condition)

let test_or =
  test
    ~config:Config.(max_size 100)
    (fun () ->
      let* e1 = Sample.with_log "e1" pp sample
      and* e2 = Sample.with_log "e2" pp sample in
      equal Comparator.bool (eval e1 || eval e2) (eval (Or (e1, e2))))

let suite = suite [ ("Exp and", test_and); ("Exp or", test_or) ]
