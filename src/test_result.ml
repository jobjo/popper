open Format

type error =
  { num_shrinks : int
  ; pp : Format.formatter -> unit -> unit
  ; explanation : string
  }

type result =
  { name : string option
  ; num_samples : int
  ; error : error option
  ; time : float
  }

type t =
  { num_passed : int
  ; num_failed : int
  ; num_discared : int
  ; results : result list
  }

let num_tests { num_passed; num_failed; num_discared; _ } =
  num_passed + num_discared + num_failed

let is_passed ({ num_failed; _ } as res) = num_tests res > 0 && num_failed = 0

(* OK: 5/5 tests passed! *)

(* OK: 4/5 tests passed, 1 discarded and 0 failed. *)

(* FAIL: 4/5 tests passed and 1 failed. *)

(* FAIL: 3/5 tests passed, 1 discared and 1 failed. *)
let green pp = Printer.pp_color Printer.green pp
let yellow pp = Printer.pp_color Printer.green pp
let red pp = Printer.pp_color Printer.red pp
let blue pp = Printer.pp_color Printer.red pp

let pp_header out ({ num_passed; num_failed; num_discared; _ } as res) =
  let num_tests = num_tests res in
  let pp_passed out () =
    fprintf
      out
      "%a/%a tests passed"
      (green pp_print_int)
      num_passed
      (blue pp_print_int)
      num_tests
  in
  let pp_discarded out () =
    fprintf out "%a discarded" (yellow pp_print_int) num_discared
  in
  let pp_failed out () =
    fprintf out "%a failed" (red pp_print_int) num_failed
  in
  (* OK: n/n tests passed!*)
  if num_passed = num_tests then
    fprintf out "%a %a!" (green pp_print_string) "OK:" pp_passed ()
  else if num_failed = 0 then
    (* OK: a/n tests passed, x discared and 0 failed!*)
    fprintf
      out
      "%a %a, %a and %a!"
      (green pp_print_string)
      "OK:"
      pp_passed
      ()
      pp_discarded
      ()
      pp_failed
      ()
  else if num_discared > 0 then
    (* FAIL: a/n tests passed, x discared and y failed.*)
    fprintf
      out
      "%a %a, %a and %a."
      (red pp_print_string)
      "FAIL:"
      pp_passed
      ()
      pp_discarded
      ()
      pp_failed
      ()
  else (* FAIL: a/n tests passed and x failed. *)
    fprintf
      out
      "%a %a and %a."
      (red pp_print_string)
      "FAIL:"
      pp_passed
      ()
      pp_failed
      ()

(*
 OK   Test foo -> testing bar   Passed 200 samples.       0.020ms
 OK   Test zoo                  Passed 200 samples.       0.001ms
 FAIL Test apa                  Failed after 23 sample.   0.001ms
 OK   Test foo -> testing bar   Passed 200 samples.       0.020ms
*)
let pp_results out res =
  let to_row { name; num_samples; error; time } =
    let status =
      if Option.is_some error then
        Table.text ~color:Printer.red "FAIL"
      else
        Table.text ~color:Printer.green "OK"
    in
    let name =
      Table.text ~color:Printer.green @@ Option.fold ~none:"" ~some:Fun.id name
    in
    let num_samples =
      let msg =
        match error with
        | Some { explanation; _ } -> explanation
        | None ->
          Printf.sprintf
            "Passed %d %s"
            num_samples
            (if num_samples = 0 then "sample" else "samples")
      in
      Table.text msg
    in
    let time = Table.text (Printf.sprintf "%2fms" time) in
    [ status; name; num_samples; time ]
  in
  let columns = [ Table.left; Table.left; Table.left; Table.right ] in
  Table.of_list ~columns @@ List.map to_row res |> Table.pp out

let pp_failed_results out res =
  List.iter
    (function
      | { name
        ; num_samples
        ; error = Some { explanation = _; num_shrinks; pp }
        ; time = _
        } ->
        let name = Option.fold ~none:"" ~some:Fun.id name in
        let pp_header out () =
          fprintf
            out
            "Failed %a after %a samples and %a shrinks."
            (blue pp_print_string)
            name
            (blue pp_print_int)
            num_samples
            (blue pp_print_int)
            num_shrinks
        in
        fprintf out "[@<v 2>@%a@,%a@]" pp_header () pp ()
      | _ -> ())
    res

let pp out ({ results; _ } as res) =
  if num_tests res = 0 then
    fprintf out "No tests run."
  else
    fprintf
      out
      "[@<v 2>%a@,%a@,%a@]"
      pp_header
      res
      pp_results
      results
      pp_failed_results
      results
