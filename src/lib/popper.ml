module Comparator = Comparator
module Input = Input
module Output = Output
module Sample = Sample
module Proposition = Proposition
module Test = Test
module Random = Random
module Consumed = Consumed
module Config = Config

exception Popper_error

let suite ts = Test.suite ts

let equal ?loc testable x y =
  Sample.return @@ Proposition.equal ?loc testable x y

let less_than ?loc testable x y =
  Sample.return @@ Proposition.less_than ?loc testable x y

let greater_than ?loc testable x y =
  Sample.return @@ Proposition.greater_than ?loc testable x y

let greater_equal_than ?loc testable x y =
  Sample.return @@ Proposition.greater_equal_than ?loc testable x y

let less_equal_than ?loc testable x y =
  Sample.return @@ Proposition.less_equal_than ?loc testable x y

let is_true ?loc b = Sample.return @@ Proposition.is_true ?loc b
let is_false ?loc b = Sample.return @@ Proposition.is_false ?loc b
let all ps = Sample.sequence ps |> Sample.map Proposition.all
let any ps = Sample.sequence ps |> Sample.map Proposition.any
let pass = Sample.return Proposition.pass
let fail ?loc s = Sample.return @@ Proposition.fail_with ?loc s

let test ?(configs = []) =
  let config = Config.all configs in
  Test.make ~config

let run ?(configs = []) t =
  let config = Config.all configs in
  if Test.run ~config t then
    ()
  else
    raise Popper_error

let check ?configs f = run ?configs (Test.make f)
