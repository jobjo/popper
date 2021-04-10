# Popper

*An OCaml testing library in the spirit of Karl Popper.*


## Examples

### A simple test-suite

```ocaml
open Popper
open Syntax

let test_rev =
  test (fun () ->
    eq ~loc:__LOC__ Comparator.(list int) (List.rev [ 1; 2; 3 ]) [ 2; 3; 1 ])

let test_rev_twice =
  let open Generator in
  test (fun () ->
    let* xs = list int in
    is_true ~loc:__LOC__ (List.rev (List.rev xs) = xs))

let suite = suite [ ("Reverse", test_rev); ("Reverse twice", test_rev_twice) ]
let () = run suite
```

When run gives:

![image](https://user-images.githubusercontent.com/820478/113290657-dc8a0480-92e9-11eb-9b18-5bbe30e731c9.png)


### Deriving generators


```ocaml
open Popper
open Syntax

type t =
  | Lit of bool
  | And of t * t
  | Or of t * t
  | Not of t
[@@deriving show, eq, popper]

let rec eval = function
  | Lit b -> b
  | And (e1, e2) -> eval e1 || eval e2
  | Or (e1, e2) -> eval e1 || eval e2
  | Not b -> not @@ eval b

let test_and =
  let open Generator in
  test (fun () ->
    let* e1 = generate in
    let* e2 = generate in
    let* () = log_key_value "e1" (show e1) in
    let* () = log_key_value "e2" (show e2) in
    let condition = (eval e1 && eval e2) = eval (And (e1, e2)) in
    is_true ~loc:__LOC__ condition)

let () = run test_and

```

Yields:

![image](https://user-images.githubusercontent.com/820478/113483019-43015500-9499-11eb-8302-de90ce5deefc.png)

### Deriving comparators

An example of using a derived comparator.

```ocaml
open Popper
open Syntax

type t =
  { x_values : int list
  ; y_values : int list
  ; x_axis : string
  ; y_axis : string
  }
[@@deriving show, eq, popper]

let flip { x_values; y_values; x_axis; y_axis } =
  { x_values = y_values; y_values = x_values; x_axis = y_axis; y_axis = x_axis }

(* Bad test *)
let test_flip_twice =
  test (fun () ->
    let* s = generate in
    eq comparator (flip s) s) 

let suite = suite [ ("Flip chart", test_flip_twice) ]

let () = run suite
```

Gives:

![image](https://user-images.githubusercontent.com/820478/114268637-0209c300-99fa-11eb-9a67-09b2c1dedd3f.png)


### Deriving test-data (including functions)

```ocaml
open Popper

type 'a tree =
  | Leaf of 'a
  | Node of 'a tree * 'a tree
[@@deriving show, eq, popper]

let leaf x = Leaf x
let node x y = Node (x, y)

let rec map f = function
  | Leaf x -> Leaf (f x)
  | Node (a, b) -> node (map f a) (map f b)

let rec to_list = function
  | Leaf x -> [ x ]
  | Node (a, b) -> to_list a @ to_list b

type test_data =
  { tree : int tree
  ; f : int -> int
  }
[@@deriving generator]

let test_map =
  let open Syntax in
  test (fun () ->
    let* { tree; f } = generate_test_data in
    let r1 = List.map f @@ to_list tree in
    let r2 = to_list @@ map f tree in
    eq (Comparator.list Comparator.int) r1 r2)

let suite = suite [ ("Map tree", test_map) ]

let () = run suite
```

Gives:

![image](https://user-images.githubusercontent.com/820478/114268695-6036a600-99fa-11eb-9860-a8fd8ee7790e.png)


