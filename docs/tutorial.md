# Using Popper 

In the following sections we'll look at how to get started with defining and
running tests.

## Hello World

In order to use the library, you need to add a dependency on `popper`.
Popper consists of two pars:

- A library `popper` containing functions for building and executing tests.

- A `ppx` part `ppx_deriving_popper` which allows you to derive useful
*sampler* and *comparators* for custom data-types.

We'll cover the `ppx` in a bit, but for now it's enough to know that by
adding the preprocessor dependency on `ppx_deriving_popper`, the runtime
library `popper` included as well.

Currently also need need to add dependencies for deriving `show` and
`ord` for your data types. Your dune file may look like the following:


```clojure
(test
 (name run)
 (preprocess
  (pps ppx_deriving.show ppx_deriving.ord ppx_deriving_popper)))
```

Here's a first example of how to define a simple unit test, verifying that
that the function `List.rev` behaves as expected.  At least when applied on
the list of integers `[1; 2; 3]`.


```ocaml 
open Popper

let () =
  check @@ fun () ->
    equal Comparator.(list int) (List.rev [ 1; 2; 3 ]) [ 3; 2; 1 ]
```

When run with `dune runtest`, it produces the following output:

```
PASS: 1/1 tests passed in 0.00s.
  
  ✓  Anonymous  Passed  0ms  
```


The function `check` is for running a single (anonymous) test.  It has the
following signature:

```ocaml skip
val check : ?⁠config:Config.t -> (unit -> Proposition.t Sample.t) -> unit
```

As you can see, it takes a function that produces a value of type
`Proposition.t Sample.t` and runs it.

We'll talk more about *samples* in a bit.  The `Proposition.t` value is the
result of the test, and is one of the following:

- `pass` — the test passed.
- `fail` — the test failed.
- `discarded` — the test neither passed nor failed.

The function `equal` is used for comparing two values using a given
`Comparator.t`.  A `Comparator.t` value consists of a `compare` function and
a pretty printer.  In this case we construct an `int list Comparator.t` value
using the `list` combinator in the `Comparator` module.

Let's see what happens when a test fails. Modifying the test slightly: 

```ocaml
let () =
  check @@ fun () ->
    equal Comparator.(list int) (List.rev [ 1; 2; 3 ]) [ 2; 3; 1 ]
```

When run, it yields:

```
FAIL: 0/1 tests passed and 1 failed in 0.00s.
  
  ✖  Anonymous  Failed  0ms  
  
  Reason:
    
    [3,2,1] <> [2,3,1]

Fatal error: exception Popper.Popper_error
```

## Property-based tests

A *property-based test* is one that, instead of verifying concrete instances,
it verifies that a certain *property* holds for a large number sampled data.
The technique was popularized by the Haskell library
[QuickCheck](https://hackage.haskell.org/package/QuickCheck).

In Popper, there is no fundamental difference between unit tests and property
based tests. A simple example of the latter is a test that verifies that
reversing a list twice results in the same list. It can be expressed as follows:

```ocaml
open Sample.Syntax

let () =
  check @@ fun () ->
    let* xs = Sample.(list int) in
    equal Comparator.(list int) (List.rev (List.rev xs)) xs
```

Which yields:

```
PASS: 1/1 tests passed in 0.02s.
  
  ✓  Anonymous  Passed 300 samples  13ms  
```

In the test, a binding `xs` that draws a sample list of integers is defined.
The test returns a proposition that asserts that reversing xs twice results
in the same list as `xs`.

The function returns in the `Sample.t` context, why we can use the `let*`
syntax defined in the `Sample.Syntax` module in order to write this in a
declarative way.

As you can read from the output, when run, `check` evaluates the property
multiple times (300 by default), by drawing different samples and checking
the resulting proposition for each of them.  We'll soon take a look at what
happens when a property fails.

Note that although a property-based test such as the one above is run multiple
times, a unit-test is only run once. How does the `check` function know 
how to distinguish between the two? The difference is that a unit test does
not consume any input used for generating sample data. Any property based
test must consume some data for drawing samples. A value of type `a Sample.t`
is really a parser that reads input from a stream and produces a value of
type `a` where the value produced depends on the values in the input stream.

## Test suites

The function `check` is convenient for quickly running individual tests but when
working with multiple tests it's best to group them and run them together.

There are three essential functions you need to use:

- `test` for creating a test.
- `suite` for naming and bundling tests together.
- `run` for running a test (or a test suite).

Their signatures are:

```
val test : ?⁠config:Config.t -> (unit -> Proposition.t Sample.t) -> Test.t
val suite : (string * Test.t) list -> Test.t
val run : ?⁠config:Config.t -> Test.t -> unit
```

Here's how  you can convert both examples from above to a suite and run it:

```ocaml
let test_rev =
  test @@ fun () ->
    equal Comparator.(list int) (List.rev [ 1; 2; 3 ]) [ 3; 2; 1 ]

let test_rev_twice =
  test @@ fun () ->
    let* xs = Sample.(list int) in
    equal Comparator.(list int) (List.rev (List.rev xs)) xs

let rev_suite = suite [("Reverse", test_rev); ("Reverse twice", test_rev_twice)]

let () = run rev_suite
```

Hopefully the definitions are straight-forward.  There are two tests which
are defined using the function `test`. They are packed together via `suite`
and executed with `run`.

This results in the output:

```
PASS: 2/2 tests passed in 0.01s.
  
  ✓  Reverse        Passed               0ms  
  ✓  Reverse twice  Passed 300 samples  15ms  
```

Note that the value `rev_suite` has the same type as `test_rev` and
`test_rev_twice`, namely `Test.t`.  That means that test suites can be
nested.

## Deriving samples

For property-based tests we often want to be able to sample data of some
arbitrary type and use the values for verifying some properties.  You have
two options in that case:

1) Sample the data and define comparators manually.  
2) Derive *sample* and *comparator* functions using the `ppx_deriving_popper` ppx.

To take a concrete example, consider a simple data type `exp` for representing 
boolean expression:

```ocaml skip
type exp =
  | Lit of bool
  | And of exp * exp
  | Or of exp * exp
  | Not of exp
```

And an evaluation function `eval`, with the signature:


```ocaml skip
val eval : exp -> bool
```

What if we want to write a test that verifies that `Lit false` is the identity
for `Or`? That is, for any expression `e`, the proposition: 

```ocaml skip
eval e = eval (Or (Lit false, e))
```

The problem is that we need to be able to sample `exp` values.  We'll cover
how to do this manually later but the easiest way forward is to simply derive
it.  All we need is to derive `popper` (which additionally requires deriving
`ord` and `show` for the *comparator* part). The `ppx`s are added like so:

```ocaml
type exp =
  | Lit of bool
  | And of exp * exp
  | Or of exp * exp
  | Not of exp
[@@deriving show, ord, popper]
```

At the preprocessing stage, the ppxs generate the additional functions:


```ocaml skip
val pp_exp : Formatter.format -> exp -> unit (* from show *)
val compare_exp : exp -> exp -> int (* from ord *)
val exp_comparator : exp Comparator.t (* from popper *)
val sample_exp : exp Sample.t (* from popper *)
```

Before writing the property-based test, here's (a buggy) implementation of
`eval`:

```ocaml
let rec eval = function
  | Lit b -> b
  | And (e1, e2) -> eval e1 && eval e2
  | Or (e1, e2) -> eval e1 || eval e2
  | Not b -> not @@ eval b
```

Using the `sample_exp` function, the test is straight forward:


```ocaml skip
let test_or =
  test @@ fun () ->
    let* e = sample_exp in
    is_true (eval e = eval (Or(Lit false, e)))

let () = run test_or
```

The function `is_true` is used for verifying that the given condition is `true`.

This time the property fails and after 5 succesful samples. That is a counter-example
that resulted in the proposition failing, was found.


```
FAIL: 0/1 tests passed and 1 failed in 0.00s.
  
  ✖  Anonymous  Failed after 5 samples  2ms  
  
┌────────────────────────────────────────────────────────────────────────────────────────┐
│ Failed Anonymous after 5 samples and 7 shrinks.                                        │
└────────────────────────────────────────────────────────────────────────────────────────┘

  Reason:
    
    Expected true but got false.
```

Whenever a failing sample is encountered, the `run` function attempts to find
a *smaller* counter example by shrinking the input.  More on that later but
for now you may note that it doesn't help much as all we know is that the
proposition failed because the boolean expression evaluated to `false`.  In
order to actually see what expression was generated, we need to add some
logging. The simplest way is to use the function `Sample.with_log`, as in:


```ocaml skip
let test_or =
  test @@ fun () ->
    let* e = Sample.with_log "e" pp_exp sample_exp in
    is_true (eval e = eval (Or(Lit false, e)))

let () = run test_or
```

The `with_log` function takes a name, here `"e"`.  We must also pass a
pretty-printer for `exp` values.  The function `pp_exp` is generated from the
`ppx_deriving.show` ppx.  The last argument is a sample value to enhance.

When running the test, you now also see the logged values in the output for the
failing test:


```
FAIL: 0/1 tests passed and 1 failed in 0.00s.
  
  ✖  Anonymous  Failed after 5 samples  2ms
  
┌────────────────────────────────────────────────────────────────────────────────────────┐
│ Failed Anonymous after 5 samples and 7 shrinks.                                        │
└────────────────────────────────────────────────────────────────────────────────────────┘

  Reason:
    
    Expected true but got false.
  
  Log:
    
    e = (Lit true)
```

## Configurations

As you may have noticed, the functions `test` and `run` take an optional
configuration argument called `config`.  This allows overriding some
settings.  Either at the level of an individual test or for all tests when
passed to `run`. In case there are overlapping settings, the ones defined at the
lowest level, i.e. the ones passed to `test` take precedence.

Here are a few examples of what can be configured:

- `num_samples` — for specifying the number of samples to be drawn for property-based tests.
- `verbose` — whether or not the *logged* values should be displayed for each sample.
- `max_size` — this is a parameter that controls the maximum size of data-structures like a lists. It defaults to 100.


For the complete list, see the module `Popper.Config`.

First, let's see what happens when we run a test configured with `verbose`. Here's a modified 
version of the `test_or` function from above:

```ocaml
let test_or =
  test ~config:Config.verbose @@ fun () ->
    let* e = Sample.with_log "e" pp_exp sample_exp in
    is_true (eval e = eval (Or(Lit false, e)))

let () = run test_or
```

You now see what all the drawn expression samples look like:

```
Anonymous
  
  Sample 1:
    
    e = (Lit false)
  
  ...
  
  Sample 6:
    
    e
      =
      (And (
         (And ((Lit true),
            (And ((Lit false), (Lit false))))),
         (Or (
            (Not
               (And ((Lit true), (Lit false)))),
            (And ((Lit false), (Lit false)))))
         ))
  ...
```

Here's how to also change the number of samples to be tested:

```ocaml
let test_or =
  test ~config:Config.(all [verbose; num_samples 1000]) @@ fun () ->
    let* e = Sample.with_log "e" pp_exp sample_exp in
    is_true (eval e = eval (Or(Lit false, e)))

let () = run test_or
```

The output confirms that the number of samples considered is now 1000:

```
PASS: 1/1 tests passed in 0.05s.
  
  ✓  Anonymous  Passed 1000 samples  47ms  
```

## Custom samples

It is not always feasible to derive generic sample functions for the data-types
you need for testing.  Specifically when:

1) The data-type is abstract.
2) There are invariants that the samples must satisfy.

In those cases you may need to write your own samplers.

To give an example, consider testing an module, `Image`, with the following 
signature:

```ocaml skip
type color = White | Black | Transparent
type t 
val make : width:int -> height:int -> (x:int -> y:int -> color) -> t
val render : t -> string
val invert : t -> t
```

Here's an implementation:

```ocaml
module Image = struct
  type color = White | Black | Transparent
  type t =
    { width : int
    ; height : int
    ; get_pixel : x:int -> y:int -> color
    }

  let make ~width ~height get_pixel = { width; height; get_pixel }

  let render { width; height; get_pixel } =
    List.init height (fun y ->
      List.init width (fun x ->
        match get_pixel ~x ~y with
        | White -> "w "
        | Black -> "b "
        | Transparent -> "- ")
      |> String.concat "")
    |> String.concat "\n"

  let invert { width; height; get_pixel } =
    let get_pixel ~x ~y =
      match get_pixel ~x ~y with
      | White -> Black
      | Black -> White
      | Transparent -> White
    in
    { width; height; get_pixel }
end
```

Now say you would like to write a test for a property that states that
inverting the image twice results in the same image when rendered.  It is not
possible to derive a sample function for the abstract type `Image.t`.
Further, even if the type was exposed we'd need to satisfy some constraints
about the `width` and the `height` parameters.  However, writing the sample
function yourself is straight forward — it's just a matter of generating the
different parameters.  Here is a version where the width and height varies
between `0` and `15`:


```ocaml
let sample_img =
  let* width = Sample.Int.range 0 15 in
  let* height = Sample.Int.range 0 15 in
  let* lookup =
    Sample.fn
      (Sample.one_value_of [ Image.Black; Image.White; Image.Transparent ])
  in
  let get_pixel ~x ~y = lookup (x, y) in
  Sample.return (Image.make ~width ~height get_pixel)
```

And here is how to use the function for creating a 

```ocaml skip
let test_invert_twice =
  test @@ fun () ->
    let* img = sample_img in
    equal
      Comparator.string
      (Image.render img)
      (Image.render (Image.invert (Image.invert img)))

let suite = suite [ ("Invert twice", test_invert_twice) ]
let () = run suite
```

Running the test reveals a bug:


```
FAIL: 0/1 tests passed and 1 failed in 0.01s.
  
  ✖  Invert twice  Failed after 1 sample  10ms  
  
┌────────────────────────────────────────────────────────────────────────────────────────┐
│ Failed `Invert twice' after 1 sample and 12 shrinks.                                   │
└────────────────────────────────────────────────────────────────────────────────────────┘

  Reason:
    
    -  <> b 
  
  
Fatal error: exception Popper.Popper_error
```

# A note about shrinking

An important feature of Popper is that shrinking is embedded. What it means is that
when the `run` or `check` function finds a counter-example and attempts to shrink it,
the invariants that were used when constructing the sample are always respected.
In the example above, with the `Image.t` sample, it means that shrinking will never lead
to say a `height` value that is negative.

The reason it works is that samplers (`Sample.t` values) are are really
functions that parses an input stream.  Shrinking is performed by attempting
to simplify the input rather than the parsed values.  Simplifying the input
is done either by removing some sections or replacing it with smaller values.
