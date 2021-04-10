module Comparator = Comparator
module Generator = Generator
module Proposition = Proposition
module Test = Test
module Random = Random
module Input = Input
module Consumed = Consumed
module Output = Output
module Tag = Tag
module Syntax = Generator.Syntax

let test ?count = Test.make ?count
let suite ts = Test.suite ts
let run ?seed t = Test.run ?seed t

let eq ?loc testable x y =
  Generator.return @@ Proposition.equal ?loc testable x y

let is_true ?loc b = Generator.return @@ Proposition.is_true ?loc b
let is_false ?loc b = Generator.return @@ Proposition.is_false ?loc b
let all ps = Generator.sequence ps |> Generator.map Proposition.all
let any ps = Generator.sequence ps |> Generator.map Proposition.any
