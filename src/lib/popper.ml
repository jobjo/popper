module Comparator = Comparator
module Input = Input
module Output = Output
module Sample = Sample
module Proposition = Proposition
module Test = Test
module Consumed = Consumed
module Config = Config

exception Test_failure

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
let discard = Sample.return Proposition.discard
let test ?(config = Config.default) = Test.make ~config

let run ?(config = Config.default) t =
  if Test.run ~config t then
    ()
  else
    raise Test_failure

let check ?config f = run ?config (test ?config f)
