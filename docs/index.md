# About

> *In so far as a scientific statement speaks about reality, it must be
> falsifiable; and in so far as it is not falsifiable, it does not speak about
> reality*.

— Karl Popper

[Popper](https://github.com/jobjo/popper) is an OCaml testing library that can
be used for writing simple *unit-tests* as well as *property-based* ones. Its
underlying design is inspired by the Python library
[Hypothesis](https://hypothesis.readthedocs.io/en/latest/). 

!!! warning
    Popper is currently in alpha-stage and is to be used at your own risk.

High-level features of Popper include:

- A uniform API for defining regular unit- and property-based tests.
- Embedded shrinking — invariants that were used when constructing samples for property-based tests are always respected.
- Compositional design — tests may be bundled and nested arbitrarily.
- Ships with a `ppx` for automatically deriving *comparator* and *sample* functions for custom data types.  
- Deterministic (and reproducible) results.
- Colorful output (cred goes to [Alcotest](https://github.com/mirage/alcotest), couldn't resist the inspiration).
- Support for line-number reporting, timing information and logging.

## Learn

- Check out the [getting started](getting_started) section for a step by step introduction.
- Take a look at some [examples](https://github.com/jobjo/popper/tree/main/examples).
- Browse the [API](https://jobjo.github.io/popper/api/).
- View the [source repository](https://github.com/jobjo/popper).

## Show me an example

Here's what test output looks like:

<img src="https://user-images.githubusercontent.com/820478/117573669-2deb9780-b0d1-11eb-842d-fcc7648d8985.png"/>

It was generated from the following code:

```ocaml
open Popper
open Sample.Syntax

type exp =
  | Lit of bool
  | And of exp * exp
  | Or of exp * exp
  | Not of exp
[@@deriving show, ord, popper]

(* A buggy evaluator function *)
let rec eval = function
  | Lit b -> b
  | And (e1, e2) -> eval e1 || eval e2
  | Or (e1, e2) -> eval e1 || eval e2
  | Not b -> not @@ eval b

(* A simple unit test *)
let test_hello_world =
  test @@ fun () ->
    equal Comparator.string "hello world" (String.lowercase_ascii "Hello World")

(* Another unit test *)
let test_lit_true = test @@ fun () -> is_true (eval (Lit true) = true)

(* A property-based test *)
let test_false_ident_or =
  test @@ fun () ->
    let* e = exp_sample in
    is_true (eval e = eval (Or (Lit false, e)))

(* Another property-based test *)
let test_true_ident_and =
  test @@ fun () ->
    let* e = Sample.with_log "e" pp_exp exp_sample in
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