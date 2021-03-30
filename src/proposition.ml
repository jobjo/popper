type t =
  | Pass
  | Fail of
      { pp : Format.formatter -> unit -> unit
      ; location : string option
      }
  | Discard

let pass = Pass
let fail ?loc pp = Fail { pp; location = loc }
let fail_with ?loc s = fail ?loc (fun out () -> Format.fprintf out "%s" s)

let equal ?loc testable x y =
  if Comparable.equal testable x y then
    pass
  else
    let pp out () =
      let pp = Comparable.pp testable in
      Format.fprintf out "@[<hv>%a @,<>@;%a@]" pp x pp y
    in
    fail ?loc (Printer.red pp)

let fail_expected ?loc e v =
  let pp out () =
    Format.fprintf
      out
      "Expected %a but got %a."
      (Printer.blue Format.pp_print_string)
      e
      (Printer.red Format.pp_print_string)
      v
  in
  fail ?loc pp

let is_true ?loc b =
  if b then
    Pass
  else
    fail_expected ?loc "true" "false"

let is_false ?loc b =
  if not b then
    Pass
  else
    fail_expected ?loc "false" "true"

let and_ p1 p2 =
  match p1, p2 with
  | Pass, Pass -> Pass
  | Pass, Fail pp -> Fail pp
  | Fail fl, Pass -> Fail fl
  | Fail { pp = pp1; location }, Fail { pp = pp2; location = _ } ->
    Fail
      { pp =
          (fun out () ->
            Format.fprintf out "@[<hv>%a@,@;and@;@;%a@]" pp1 () pp2 ())
      ; location
      }
  | p, Discard -> p
  | Discard, p -> p

let or_ p1 p2 =
  match p1, p2 with
  | Pass, _ -> Pass
  | _, Pass -> Pass
  | Fail { pp = pp1; location }, Fail { pp = pp2; location = _ } ->
    Fail
      { pp = (fun out () -> Format.fprintf out "@[<hv>%a@,%a@]" pp1 () pp2 ())
      ; location
      }
  | p, Discard -> p
  | Discard, p -> p

let all xs = List.fold_right and_ xs Discard
let any xs = List.fold_right or_ xs Discard
