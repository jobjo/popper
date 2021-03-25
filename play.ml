type command =
  | Hello_world
  | Fasdefdsfasdfasdfasdf
  | AAsdfasdfasdf of string
  | Foo of int

type foo = { name : string }

(* Here we go *)
let foo = Hello_world

module Foo = struct
  type 'a t = string
end
