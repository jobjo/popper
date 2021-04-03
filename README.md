# Popper

*An OCaml testing library in the spirit of Karl Popper.*

## Examples

#### A simple test-suite:

```ocaml
open Popper
open Generator
open Syntax

let test_rev =
  Test.test (fun () ->
    Test.equal
      ~loc:__LOC__
      Comparator.(list int)
      (List.rev [ 1; 2; 3 ])
      [ 2; 3; 1 ])

let test_rev_twice =
  Test.test (fun () ->
    let* xs = list int in
    Test.is_true ~loc:__LOC__ (List.rev (List.rev xs) = xs))

let suite = Test.suite [ "Reverse", test_rev; "Reverse twice", test_rev_twice ]

let () = Test.run suite

```

When run gives:

![image](https://user-images.githubusercontent.com/820478/113290657-dc8a0480-92e9-11eb-9b18-5bbe30e731c9.png)


### Deriving generators


```ocaml
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
    let* () = log_key_value ~key:"e1" (show e1) in
    let* () = log_key_value ~key:"e2" (show e2) in

let () = Test.run test_and
```

Gives:

![image](https://user-images.githubusercontent.com/820478/113483019-43015500-9499-11eb-8302-de90ce5deefc.png)


