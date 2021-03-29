type t =
  | Pass
  | Fail of (Format.formatter -> unit -> unit)
  | Discard

type person =
  { name : string
  ; age : int
  }
[@@deriving show]

let is_fail = function
  | Fail _ -> true
  | _ -> false

let pass = Pass
let fail f = Fail f
let fail_with s = fail @@ fun out () -> Format.fprintf out "%s" s

let equals (pp : Format.formatter -> 'a -> unit) (x : 'a) (y : 'a) =
  if x = y then
    pass
  else
    let msg out () =
      let pp out () = Format.fprintf out "@[<hv>%a @,<>@;%a@]" pp x pp y in
      Format.fprintf out "@[<v 2>Reason:@;@;%a@]" (Printer.red pp) ()
    in
    fail msg

let is_true b =
  if b then Pass else Fail (fun out () -> Format.fprintf out "Expected `true`")

let log_input pp_input = function
  | Pass -> Pass
  | Fail pp_fail ->
    Fail
      (fun out () ->
        Format.fprintf
          out
          "@[<v 2>Input:@;@;%a@]@,@,%a"
          (Printer.blue pp_input)
          ()
          pp_fail
          ())
  | Discard -> Discard

let is_false b =
  if b then Fail (fun out () -> Format.fprintf out "Expected `false`") else Pass

let and_ p1 p2 =
  match p1, p2 with
  | Pass, Pass -> Pass
  | Pass, Fail pp -> Fail pp
  | Fail pp, Pass -> Fail pp
  | Fail pp1, Fail pp2 ->
    Fail
      (fun out () -> Format.fprintf out "@[<hv>%a@,@;and@;@;%a@]" pp1 () pp2 ())
  | p, Discard -> p
  | Discard, p -> p

let or_ p1 p2 =
  match p1, p2 with
  | Pass, _ -> Pass
  | _, Pass -> Pass
  | Fail pp1, Fail pp2 ->
    Fail (fun out () -> Format.fprintf out "@[<hv>%a@,%a@]" pp1 () pp2 ())
  | p, Discard -> p
  | Discard, p -> p

let all xs = List.fold_right and_ xs Discard
let any xs = List.fold_right or_ xs Discard

let pp out = function
  | Pass -> Format.fprintf out "Pass"
  | Fail pp -> Format.fprintf out "@[<v 2>Proposition failed:@,@,%a@]" pp ()
  | Discard -> Format.fprintf out "Discard"

let show = pp Format.std_formatter
let p1 = equals pp_person { name = "Aa"; age = 32 } { name = "Aa"; age = 32 }
let p2 = equals pp_person { name = "C"; age = 32 } { name = "C"; age = 32 }
let main () = show (all [ p1; p2 ])
