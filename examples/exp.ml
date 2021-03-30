open Popper
open Generator.Syntax

type t =
  | Lit of bool
  | And of t * t
  | Or of t * t
  | Not of t
[@@deriving show]

let rec eval = function
  | Lit b -> b
  | And (e1, e2) -> eval e1 || eval e2
  | Or (e1, e2) -> eval e1 || eval e2
  | Not b -> not @@ eval b

let gen =
  let open Popper.Generator in
  let rec aux size () =
    let lit = one_value_of [ Lit true; Lit false ] in
    let and_ () =
      let* e1 = delayed (aux (size * 2)) in
      let* e2 = delayed (aux (size * 2)) in
      return @@ And (e1, e2)
    in
    let or_ () =
      let* e1 = delayed (aux (size * 2)) in
      let* e2 = delayed (aux (size * 2)) in
      return @@ And (e1, e2)
    in
    let not_ () = map (fun x -> Not x) (delayed (aux (size + 1))) in
    if size > 5 then
      lit
    else
      one_of [ lit; delayed and_; delayed or_; delayed not_ ]
  in
  delayed (aux 1)

let test_and =
  Test.test
    (let* e1 = gen in
     let* e2 = gen in
     let* () = Generator.log (show e1) in
     let* () = Generator.log (show e2) in
     Test.is_true ((eval e1 && eval e2) = eval (And (e1, e2))))

let test_or =
  Test.test
    (let* e1 = gen in
     let* e2 = gen in
     Test.is_true ((eval e1 || eval e2) = eval (Or (e1, e2))))

let suite = Test.suite [ "Exp and", test_and; "Exp or", test_or ]
