# Popper

*Popper* (after Karl) is an OCaml library that can be used for writing simple *unit-tests*
as well as *property-based* ones. Its underlying design is inspired by the Python library
[Hypothesis](https://hypothesis.readthedocs.io/en/latest/). 

The API for defining and running test suites also draws inspiration from
existing OCaml testing libraries, in particular
[Alcotest](https://github.com/mirage/alcotest),
[OUnit](https://github.com/gildor478/ounit) and
[QCheck](https://github.com/c-cube/qcheck).

Here's an example of what the output looks like:

![image](https://user-images.githubusercontent.com/820478/116737784-8f34ac00-a9e9-11eb-8130-a89adce0522f.png)


It's generated from the following test-spec:

```ocaml
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

(* Another property-based test *)
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
```

The high-level features of the library are:

- A uniform API for defining unit-tests and property-based tests.
- Compositional design — tests and test suites can be nested arbitrarily.
- Comes with a `ppx` for automatically deriving `comparator` and `sample` functions for test data.
- Embedded shrinking - invariants that were used when constructing test-cases for property-based tests are always respected.
- Colorful output (yes, heavily inspired by [Alcosest](https://github.com/mirage/alcotest)).
- Support for line-number reporting, timing information and debugging. 

## Getting started

Here a [tutorial](docs/tutorial.md) covering the basics getting-started steps.
Also see examples in the [examples](examples) folder.
