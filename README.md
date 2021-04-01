# Popper

*An OCaml testing library in the spirit of Karl Popper.*

## Examples

Hello-world:


```ocaml
open Popper
open Generator
open Syntax

let test_rev =
  Test.test (fun () ->
    Test.equal
      ~loc:__LOC__
      Comparable.(list int)
      (List.rev [ 1; 2; 3 ])
      [ 2; 3; 1 ])

let test_rev_twice =
  Test.test (fun () ->
    let* xs = many int in
    Test.is_true (List.rev (List.rev xs) = xs))

let suite = Test.suite [ "Reverse", test_rev; "Reverse twice", test_rev_twice ]
let () = Test.run suite

```

When run gives:

![image](https://user-images.githubusercontent.com/820478/113290657-dc8a0480-92e9-11eb-9b18-5bbe30e731c9.png)

