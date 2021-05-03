# Using Popper 

In the following sections we'll look at how to get started using the Popper
library for writing and running unit and property-based tests.

## Hello, World

In order to use the library, you need to add a dependency. Popper is
split into two parts:

- A library `popper` containing functions for building and executing tests.

- A library `ppx_deriving_popper` which allows you to derive functions
for *sampling* and comparing test data.

We'll cover the `ppx` in a bit, but for now it's enough to note that by
adding the preprocessor dependency on `ppx_deriving_popper`, the runtime
library `popper` is included as well.

Currently also need need to add dependencies for deriving `show` and
`ord` for your data types. Here's an example dune file defining a test
that requires a file `run.ml` to be present:

```clojure
(test
 (name run)
 (preprocess
  (pps ppx_deriving.show ppx_deriving.ord ppx_deriving_popper)))
```

As an example, here is a simple unit test that verifies that
that the function `List.rev` behaves as expected when applied with
the list `[1; 2; 3]`:

```ocaml 
open Popper

let () =
  check @@ fun () ->
    equal Comparator.(list int) (List.rev [ 1; 2; 3 ]) [ 3; 2; 1 ]
```

When run with `dune runtest`, it produces the following output:

![image](https://user-images.githubusercontent.com/820478/116917024-04042200-ac46-11eb-950b-dafe2575a559.png)


The function `check` is for running a single (anonymous) test.  Its signature
is:

```ocaml skip
val check : ?⁠config:Config.t -> (unit -> Proposition.t Sample.t) -> unit
```

As you can see, it takes a function that produces a value of type
`Proposition.t Sample.t` and runs it.

We'll talk more about *samples* in a bit.  The `Proposition.t` value is the
result of the test, and can take on one of the following values:

- `pass` — the test passed.
- `fail` — the test failed.
- `discarded` — the test neither passed nor failed.

The function `equal` is used for comparing two values using a given
`Comparator.t`.  A `Comparator.t` value consists of a `compare` function and
a pretty printer.  In this case we construct an `int list Comparator.t` value
using the `list` combinator in the `Comparator` module.

Let's see what happens when a test fails.  Modifying the example above
slightly:

```ocaml 
let () =
  check @@ fun () ->
    equal Comparator.(list int) (List.rev [ 1; 2; 3 ]) [ 2; 3; 1 ]
```

This now yields:

![image](https://user-images.githubusercontent.com/820478/116917152-3281fd00-ac46-11eb-9316-c1228505ec33.png)


## Property-based tests

A *property-based test* is one that, instead of verifying concrete instances,
it confirms that a certain *property* holds for a large number sampled data.
The technique was popularized by the Haskell library
[QuickCheck](https://hackage.haskell.org/package/QuickCheck).

In Popper, there is no fundamental difference between unit tests and property
based tests.  A concrete example is a test that verifies that reversing a
list twice results in the same list.  Using Popper, it can be expressed as
follows:

```ocaml 
open Popper
open Sample.Syntax

let () =
  check @@ fun () ->
    let* xs = Sample.(list int) in
    equal Comparator.(list int) (List.rev (List.rev xs)) xs
```

Which yields:

![image](https://user-images.githubusercontent.com/820478/116917398-842a8780-ac46-11eb-8bfc-154cef57529a.png)


In the test, `xs` is a sample of list of integers.  The test returns a
proposition that asserts states that reversing `xs` twice gives the
same list back.

The function returns in the `Sample.t` context.  The `let*` syntax defined in
the `Sample.Syntax` module provides a convenient method for expressing this
in a declarative way.

As you can read from the output, when run, `check` evaluates the property
multiple times (300 by default), by drawing different samples and checking
the resulting proposition for each of them.  To see what happens when a test
fails, here's an example of a test for verifying that `List.sort` returns a
sorted list, but where the condition does not account for lists containing
duplicates:


```ocaml
let rec is_sorted = function
  | x :: y :: ys -> x < y && is_sorted (y :: ys)
  | _ -> true

let () =
  check @@ fun () ->
    let* xs = Sample.(list int) in
    is_true (is_sorted @@ List.sort Int.compare xs)
```

The function `is_true` is used for a proposition that states that the given
condition is `true`.  If `false` is given, the proposition is the `fail`
value.

Running the test results in the following error:

![image](https://user-images.githubusercontent.com/820478/116917535-ade3ae80-ac46-11eb-8a92-7b0d10a67fd8.png)

Whenever a failing sample is encountered, the `run` function attempts to find
a *smaller* counter example by shrinking the input stream.  However, it does
not help much much as all we know is that the proposition failed because the
boolean condition was `false`.  In order to actually see the drawn sample for
which the condition `is_sorted` did not hold, we need to add some logging.
The simplest way is to use the function `Sample.with_log`, which has the
signature:


```
val with_log : string -> (Stdlib.Format.formatter -> 'a -> unit) -> 'a Sample.t -> 'a Sample.t
```

It takes a string value for the key, a pretty printer and a sampler and it returns
a new `Sample.t` value that also records the sampled values.

In order to get pretty printer for list, we can derive it using `[@@deriving
show]`.  It's also possible to derive the sampler, which we'll look at later.
For now:

```ocaml
type int_list = int list [@@deriving show]

let () =
  check @@ fun () ->
    let* xs = Sample.(with_log "xs" pp_int_list (list int)) in
    is_true (is_sorted @@ List.sort Int.compare xs)
```

This yields:

![image](https://user-images.githubusercontent.com/820478/116917809-0c109180-ac47-11eb-8096-1896a1d832f1.png)


The log section displays the sample value `xs` for which the test failed. 

Note that although a property-based test such as the one above is run
multiple times, a unit-test is only run once.  How does the `check` function
know how to distinguish between the two?  The answer is that the unit test
does not consume any input used for generating sample data.  Any
property-based test must consume some data for drawing (pseudo-random)
samples.  A value of type `a Sample.t` is really a parser that reads input
from a stream of numbers (`int32` values) and produces a result of type `a`.
By varying the input stream we feed to the parser, we get different results
back.

## Test suites

The function `check` is convenient for quickly running individual tests but when
working with multiple tests it's best to bundle and run them together.

There are three essential functions you need to know about:

- `test` for creating a test.
- `suite` for naming and grouping tests together.
- `run` for running a test (or a test suite).

Their signatures are:

```
val test : ?⁠config:Config.t -> (unit -> Proposition.t Sample.t) -> Test.t
val suite : (string * Test.t) list -> Test.t
val run : ?⁠config:Config.t -> Test.t -> unit
```

Here's how you can combine the tests from above in a suite, and run it:

```ocaml
let test_rev =
  test @@ fun () ->
    equal Comparator.(list int) (List.rev [ 1; 2; 3 ]) [ 3; 2; 1 ]

let test_rev_twice =
  test @@ fun () ->
    let* xs = Sample.(list int) in
    equal Comparator.(list int) (List.rev (List.rev xs)) xs

let test_sort =
  test @@ fun () ->
    let* xs = Sample.(with_log "xs" pp_int_list (list int)) in
    is_true (is_sorted @@ List.sort Int.compare xs)

let tests =
  suite 
    [ ("Reverse", test_rev)
    ; ("Reverse twice", test_rev_twice)
    ; ("Sort", test_sort)
    ]

let () = run tests
```

Hopefully the definitions are straight-forward.  There are three tests which
are defined using the function `test`. They are packed together via `suite`
and executed with `run`.

This results in the output:

![image](https://user-images.githubusercontent.com/820478/116917989-411ce400-ac47-11eb-808c-3e062331e45a.png)


Note that the value `tests` has the same type as `test_rev` ,
`test_rev_twice`, and `test_sort`, namely `Test.t`.  That means that test
suites may also be nested.  For instance, as in:

```ocaml
let rev_suite =
  suite [ ("Simple list", test_rev); ("Reverse twice", test_rev_twice) ]

let tests = suite [ ("Reverse", rev_suite); ("Sort", test_sort) ]

let () = run tests
```

Which gives:

![image](https://user-images.githubusercontent.com/820478/116918131-6d386500-ac47-11eb-95c6-bc518f039952.png)


## Deriving samples

For property-based tests we sample data of some type and use the values for
verifying properties.  You have two options in that case:

1) Sample the data and define comparators manually (as in the above examples).
2) Derive *sample* and *comparator* functions using the `ppx_deriving_popper` ppx.

For example, consider a simple data type, `exp`, for representing boolean
expressions:

```ocaml skip
type exp =
  | Lit of bool
  | And of exp * exp
  | Or of exp * exp
  | Not of exp
```

Along with an evaluation function, `eval`, with a signature:


```ocaml skip
val eval : exp -> bool
```

What if we want to write a test that verifies that `Lit false` is the
identity expression for `Or`?  That is, for any expression `e`, the
proposition:

```ocaml skip
eval e = eval (Or (Lit false, e))
```

should evaluate to true.

The problem is that we need to be able to sample `exp` values.  We'll cover
how to do this manually later but the easiest way forward is to derive it.
All we need is to add an attribute `[@@deriving .. ]`.  It also
requires deriving `ord` and `show` for the *comparator* part:

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


```ocaml
let test_or =
  test @@ fun () ->
    let* e = sample_exp in
    is_true (eval e = eval (Or(Lit false, e)))

let () = run test_or

```

The test now fails and after 5 successful samples:

![image](https://user-images.githubusercontent.com/820478/116918459-d324ec80-ac47-11eb-829d-cc854a55e307.png)


Again, in order to actually view the expression that was generated in order
for the proposition to fail, we need to add logging:

```ocaml
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

![image](https://user-images.githubusercontent.com/820478/116918579-fea7d700-ac47-11eb-8abd-1ce99315ecf1.png)


### Combining propositions

Just like tests can be composed into test suites, propositions can also be 
grouped. There are two functions:


```ocaml skip
val all : Proposition.t Sample.t list -> Proposition.t Sample.t

val any : Proposition.t Sample.t list -> Proposition.t Sample.t
```

They both take a list of propositions (for convenience `Proposition.t
Sample.t` values) and collapse them into a single proposition. As the names
suggest `all` requires all propositions to pass, and `any` requires at least
one to do so, in order to return a passing result.

Here's an example:


```ocaml
let test_id =
  test @@ fun () ->
    let* e = sample_exp in
    all
      [ is_true (eval e = eval (Or (Lit false, e)))
      ; is_true (eval e = eval (And (Lit true, e)))
      ]
```

## Configurations

As you may have noticed, the functions `test` and `run` take an optional
configuration argument called `config`.  This allows overriding some
settings.  Either at the level of an individual test or for all tests when
passed to `run`. In case there are overlapping settings, the ones defined at the
lowest level, i.e. the ones passed to `test` take precedence.

Here are a few examples of what can be configured:

- `num_samples` — for specifying the number of samples to be drawn for property-based tests. The default value is 300.
- `verbose` — whether or not the *logged* values should be displayed for each sample.
- `max_size` — this is a parameter that controls the maximum size parameter for data-structures like a lists. It defaults to 100.

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

![image](https://user-images.githubusercontent.com/820478/116918839-53e3e880-ac48-11eb-97cf-4a91c85b3189.png)


Also changing the number of samples to be tested can be done by combining `Config.t` values
with the `Config.all` combinator:

```ocaml
let test_not =
  let config = Config.(all [verbose; num_samples 1000]) in
  test ~config @@ fun () ->
    let* e = Sample.with_log "e" pp_exp sample_exp in
    is_true (not (eval e) = eval (Not e))

let () = run test_or
```

The output confirms that the number of samples considered is now 1000:

![image](https://user-images.githubusercontent.com/820478/116919592-66aaed00-ac49-11eb-97dd-ccac8f823e59.png)

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
inverting the image twice results in the same image when rendered.  However,
it is not possible to derive a sample function for the abstract type
`Image.t`.  Further, even if the type had been exposed we'd need to satisfy
some invariants for the `width` and the `height` parameters.  Luckily,
writing the sample function yourself is straight forward — it's just a matter
of generating the different parameters.  Here is a version where the width
and height varies between `0` and `15`:


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

And here is how to use the function for defining a test:

```ocaml
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

![image](https://user-images.githubusercontent.com/820478/116918839-53e3e880-ac48-11eb-97cf-4a91c85b3189.png)

# A note about shrinking

An important feature of Popper is that shrinking is *embedded*. What it means is that
when the `run` or `check` function finds a counter-example and attempts to shrink it,
the invariants that were used when constructing the samples are always respected.
In the example above, with the `Image.t` sample, it means that shrinking will never lead
to, say a `height` value that is negative.

As mentioned above, a value of type `a Sample.t` is really a parser from an
input stream to an `a` value.  Shrinking is performed by attempting to
simplify the input rather than modifying the parsed values themselves.
Simplifying the input is done either by removing some sections or replacing
it with smaller values.
