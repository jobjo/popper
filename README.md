# Popper

*An OCaml testing library in the spirit of Karl Popper*

```ocaml
open Popper
open Generator
open Syntax

let suite =
  Test.test
    (let* xs = many int in
     Test.is_true (List.rev @@ List.rev xs = xs))

let () = Test.run suite
```
