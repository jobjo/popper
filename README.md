# Popper

>*In so far as a scientific statement speaks about reality, it must be
> falsifiable; and in so far as it is not falsifiable, it does not speak about
> reality.  

(Kar Popper)

*Popper* is an OCaml library that can be used for writing simple *unit-tests*
as well as *property-based* ones. Its underlying design is inspired by the Python library
[Hypothesis](https://hypothesis.readthedocs.io/en/latest/). 

High-level features of Popper include:

- A uniform API for defining unit and property-based tests.
- Compositional design — tests and test suites can be nested arbitrarily.
- Ships with a `ppx` for automatically deriving *comparator* and *sample* functions for custom data types.
- Embedded shrinking — invariants that were used when constructing test data for property-based tests are always respected.
- Colorful output (sorry [Alcotest](https://github.com/mirage/alcotest), but couldn't resist the inspiration).
- Support for line-number reporting, timing information and debugging. 

## Getting started

- Check out [this tutorial](docs/tutorial.md) for a step by step introduction of the various features and the API.

- See examples in the [examples](examples) folder.

## An example

Here's what test output might look like:

![image](https://user-images.githubusercontent.com/820478/116737784-8f34ac00-a9e9-11eb-8130-a89adce0522f.png)

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

## Contributing

TODO.

## Comparing with other libraries

Popper is designed with the following objectives in mind:

1. Make it as seamless as possible to write property-based tests — for instance by using a ppx to derive custom sample functions.
2. Use embedded shrinking (ala [Hypothesis](https://hypothesis.readthedocs.io/en/latest/)) and eliminate the need for writing *shrinkers* manually.

The property based aspects overlap with the existing libraries [QCheck](https://github.com/c-cube/qcheck) and
[Crowbar](https://github.com/stedolan/crowbar).

Popper also supports writing simple unit tests and the ability to compose
tests into suites.  This API and the output is partly inspired by the unit
testing library [Alcotest](https://github.com/mirage/alcotest).

Here's a table comparing features across different OCaml testing libraries:


| Library                                           | Test suites   | Property-based | Embeded shrinking | PPX generators | Fuzzying
| --------------------------------------------------|:-------------:|:--------------:|:-----------------:|:--------------:|:---------:|
| Popper                                            | ✅            | ✅              | ✅                | ✅             | ❌ 
| [Alcotest](https://github.com/mirage/alcotest)    | ✅            | ❌              |                   | ❌             | 
| [OUnit](https://github.com/gildor478/ounit)       | ✅            | ❌              |                   | ❌             | 
| [QCheck](https://github.com/c-cube/qcheck)        | ✅            | ✅              |                   | ❌             |
| [Crowbar](https://github.com/stedolan/crowbar)    | ❌            | ✅              | ❌                | ❌              | ✅  

It might be possible to write some adaptors to be able to integrate with
these libraries but nothing such exists at the moment.