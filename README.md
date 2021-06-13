<div align="center" >
<img width="50%" src="docs/img/logo.png"/>

# Property-based testing at ease

</div>

[![Main workflow](https://github.com/jobjo/popper/workflows/Main%20workflow/badge.svg?branch=main)](https://github.com/jobjo/popper/actions)
[![docs](https://img.shields.io/badge/doc-online-blue.svg)](https://jobjo.github.io/popper/api)

[Popper](https://github.com/jobjo/popper) is an OCaml testing library that can
be used for writing simple *unit-tests* as well as *property-based* ones. Its
underlying design is inspired by the Python library
[Hypothesis](https://hypothesis.readthedocs.io/en/latest/).

See the [documentation page](https://jobjo.github.io/popper/) for information on
how to get started.

## Overview

High-level features of Popper include:

- A uniform API for defining regular unit- and property-based tests.
- Embedded shrinking — invariants used when constructing samples for property-based tests are always respected.
- Compositional design — tests may be bundled and nested arbitrarily.
- Ships with a `ppx` for automatically deriving *comparator* and *sample* functions for custom data types.
- Deterministic (and reproducible) results.
- Colorful output (cred goes to [Alcotest](https://github.com/mirage/alcotest), couldn't resist some inspiration).
- Support for line-number reporting, timing and logging.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for how to build and contribute to
Popper.

## Learn

- Check out the [getting started](https://jobjo.github.io/popper/getting_started) section for a step by step introduction.
- Take a look at some examples in the [examples](https://github.com/jobjo/popper/tree/main/examples) folder.
- Browse the [API docs](https://jobjo.github.io/popper/api).

## Show me an example

Here's what test output might look like:

<p align="center">
<img src="https://user-images.githubusercontent.com/820478/120936489-eb34d380-c6ff-11eb-8b3d-ce48094225a8.png"/>
</p>

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

(* Bundle some tests together *)
let exp_suite =
  suite
    [ ("Lit true", test_lit_true)
    ; ("False ident or", test_false_ident_or)
    ; ("True ident and", test_true_ident_and)
    ]

(* Top-level test-suite *)
let suite =
  suite [ ("Hello World", test_hello_world); ("Expression", exp_suite) ]

let () = run suite
```

## Comparing with other libraries

Popper is designed with the following objectives in mind:

1. Make it as seamless as possible to write property-based tests — for instance
by using a ppx to derive *sample* functions for custom data-types.

2. Use embedded shrinking (ala
[Hypothesis](https://hypothesis.readthedocs.io/en/latest/)) and eliminate the
need for writing *shrinkers* manually.

The property-based aspects overlap with the OCaml libraries
[QCheck](https://github.com/c-cube/qcheck) and
[Crowbar](https://github.com/stedolan/crowbar).

Popper also supports writing simple unit tests and the ability to compose tests
into suites.  This API and the output is partly inspired by the testing
library [Alcotest](https://github.com/mirage/alcotest).

Here's a table comparing features across different OCaml testing libraries:


| Library                                                                   | Test suites   | Property-based | Embeded shrinking | PPX generators | Fuzzying
| --------------------------------------------------------------------------|:-------------:|:--------------:|:-----------------:|:--------------:|:---------:|
| [Popper](https://github.com/jobjo/popper)                                 | ✅            | ✅              | ✅                | ✅             | ❌
| [Alcotest](https://github.com/mirage/alcotest)                            | ✅            | ❌              | -                 | ❌             | -
| [OUnit](https://github.com/gildor478/ounit)                               | ✅            | ❌              | -                 | ❌             | -
| [QCheck](https://github.com/c-cube/qcheck)                                | ✅            | ✅              | ❌                | ❌             | ❌
| [Crowbar](https://github.com/stedolan/crowbar)                            | ❌            | ✅              | ❌                | ❌              | ✅
| [Base_quickcheck](https://opensource.janestreet.com/base_quickcheck/)     | ❌            | ✅              | ❌                | ✅              | ❌

It might be possible to write some adaptors to be able to integrate with
these libraries but nothing such exists at the moment.
