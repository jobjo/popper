open Format

type failure =
  { num_shrinks : int
  ; pp : Format.formatter -> unit -> unit
  ; explanation : string
  }

type discard = { num_discarded : int }

type status =
  | Pass
  | Fail of failure
  | Discarded of discard

type result =
  { name : string option
  ; num_passed : int
  ; status : status
  ; time : float
  ; logs : string list
  }

type t =
  { num_passed : int
  ; num_failed : int
  ; num_discarded : int
  ; time : float
  ; results : result list
  }

let result_passed { status; _ } =
  match status with
  | Pass -> true
  | _ -> false

let result_fail { status; _ } =
  match status with
  | Fail _ -> true
  | _ -> false

let result_discarded { status; _ } =
  match status with
  | Discarded _ -> true
  | _ -> false

let num_tests { num_passed; num_failed; num_discarded; _ } =
  num_passed + num_discarded + num_failed

let pp_header
  out
  ({ num_passed; num_failed; num_discarded; time; results = _ } as res)
  =
  let fail_label = "FAIL:" in
  let pass_label = "PASS:" in
  let num_tests = num_tests res in
  let pp_passed out () =
    fprintf
      out
      "%a/%a tests passed"
      (Printer.green pp_print_int)
      num_passed
      (Printer.blue pp_print_int)
      num_tests
  in
  let pp_discarded out () =
    fprintf out "%a discarded" (Printer.yellow pp_print_int) num_discarded
  in
  let pp_failed out () =
    fprintf out "%a failed" (Printer.red pp_print_int) num_failed
  in
  let pp_time = Printer.blue @@ fun out () -> fprintf out "%.2fs" time in
  (* OK: n/n tests passed!*)
  if num_passed = num_tests then
    fprintf
      out
      "%a %a in %a."
      (Printer.green pp_print_string)
      pass_label
      pp_passed
      ()
      pp_time
      ()
  else if num_failed = 0 then
    (* PASS a/n tests passed, x discared and 0 failed in ns!*)
    fprintf
      out
      "%a %a, %a and %a in %a."
      (Printer.green pp_print_string)
      pass_label
      pp_passed
      ()
      pp_discarded
      ()
      pp_failed
      ()
      pp_time
      ()
  else if num_discarded > 0 then
    (* FAIL a/n tests passed, x discared and y failed in ns.*)
    fprintf
      out
      "%a %a, %a and %a in %a."
      (Printer.red pp_print_string)
      fail_label
      pp_passed
      ()
      pp_discarded
      ()
      pp_failed
      ()
      pp_time
      ()
  else (* FAIL: a/n tests passed and x failed in ns. *)
    fprintf
      out
      "%a %a and %a in %a."
      (Printer.red pp_print_string)
      fail_label
      pp_passed
      ()
      pp_failed
      ()
      pp_time
      ()

(*
 OK   Test foo -> testing bar   Passed 200 samples.       0.020ms
 OK   Test zoo                  Passed 200 samples.       0.001ms
 FAIL Test apa                  Failed after 23 sample.   0.001ms
 OK   Test foo -> testing bar   Passed 200 samples.       0.020ms
*)
let pp_results out res =
  let to_row { name; num_passed; status; time; _ } =
    let status_cell =
      match status with
      | Pass -> Table.text ~color:Printer.Color.green "✓"
      | Fail _ -> Table.text ~color:Printer.Color.red "✖"
      | Discarded _ -> Table.text ~color:Printer.Color.yellow "☐"
    in
    let name =
      let color =
        match status with
        | Fail _ -> Printer.Color.red
        | _ -> Printer.Color.blue
      in
      Table.text ~color @@ Option.fold ~none:"" ~some:Fun.id name
    in
    let num_passed =
      let msg =
        match status with
        | Pass ->
          Printf.sprintf
            "Passed %d %s"
            num_passed
            (if num_passed = 1 then "sample" else "samples")
        | Fail { explanation; _ } -> explanation
        | Discarded { num_discarded } ->
          Printf.sprintf "Passed %d and %d discarded" num_passed num_discarded
      in
      Table.text ~color:Printer.Color.faint msg
    in
    let time =
      Table.text
        ~color:Printer.Color.blue
        (Printf.sprintf "%.0fms" (time *. 1000.))
    in
    [ status_cell; name; num_passed; time ]
  in
  let columns = [ Table.left; Table.left; Table.left; Table.right ] in
  Table.of_list ~columns @@ List.map to_row res |> Table.pp out

let rendered_text_width =
  let reg = Str.regexp "\\[[0-9]+m" in
  fun s ->
    s
    |> Str.global_replace reg ""
    |> String.to_seq
    |> Seq.filter (fun c -> Char.code c <> 27)
    |> Seq.fold_left (fun n _ -> n + 1) 0

(* Inspired by Alcotest: 
https://github.com/mirage/alcotest/blob/a5d3c0e498a8706427e6337d49a1cbf235b4231d/src/alcotest-engine/pp.ml#L184 *)
let with_box (type a) f out (a : a) =
  let text_width =
    Format.kasprintf Fun.id "| %a |" f a |> rendered_text_width
  in
  let width =
    let min_box_width = 90 in
    max min_box_width text_width
  in
  let line = List.init (width - 2) (fun _ -> "─") |> String.concat "" in
  let pp_top out () = fprintf out "%s%s%s" "┌" line "┐" in
  let pp_mid out () =
    let pp_bar out () = Printer.faint pp_print_string out "│" in
    let space = String.make (width - text_width) ' ' in
    fprintf out "%a %a%s %a" pp_bar () f a space pp_bar ()
  in
  let pp_bottom out () = fprintf out "└%s┘" line in
  fprintf
    out
    "%a@.%a@.%a"
    (Printer.faint pp_top)
    ()
    pp_mid
    ()
    (Printer.faint pp_bottom)
    ()

let pp_failed_results out res =
  List.iter
    (function
      | { name
        ; num_passed
        ; status = Fail { explanation = _; num_shrinks; pp }
        ; time = _
        ; logs
        } ->
        let name = Option.fold ~none:"" ~some:(Printf.sprintf "`%s'") name in
        let pp_header out () =
          fprintf
            out
            "Failed %a after %a %s and %a shrinks."
            (Printer.red pp_print_string)
            name
            (Printer.blue pp_print_int)
            num_passed
            (if num_passed = 1 then "sample" else "samples")
            (Printer.blue pp_print_int)
            num_shrinks
        in
        let pp_logs out () =
          match logs with
          | [] -> ()
          | logs ->
            fprintf
              out
              "@,@[<v 2>Logs:@,@,%a@]"
              (fun out () ->
                List.iter
                  (fun log ->
                    log
                    |> String.split_on_char '\n'
                    |> List.iter (fun line ->
                         fprintf out "%a@," (Printer.blue pp_print_string) line);
                    fprintf out "@,")
                  logs)
              ()
        in
        fprintf
          out
          "@[<v 2>%a@[<v 2>@,%a@,%a@]@,@]@.@."
          (with_box pp_header)
          ()
          pp
          ()
          pp_logs
          ()
      | _ -> ())
    res

let pp out ({ results; _ } as res) =
  if num_tests res = 0 then
    fprintf out "No tests run."
  else
    fprintf
      out
      "@.@[<v 2>%a@,@,%a@]@;%a"
      pp_header
      res
      pp_results
      results
      pp_failed_results
      results
