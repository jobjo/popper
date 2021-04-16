open Format

type failure =
  { num_shrinks : int
  ; num_attempts : int
  ; pp : Format.formatter -> unit -> unit
  ; explanation : string
  ; location : string option
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
  ; log : Log.t
  ; verbose_log : Log.t option
  ; is_unit : bool
  }

type t =
  { num_passed : int
  ; num_failed : int
  ; num_discarded : int
  ; time : float
  ; results : result list
  }

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
      (Util.Format.green pp_print_int)
      num_passed
      (Util.Format.blue pp_print_int)
      num_tests
  in
  let pp_discarded out () =
    fprintf out "%a discarded" (Util.Format.yellow pp_print_int) num_discarded
  in
  let pp_failed out () =
    fprintf out "%a failed" (Util.Format.red pp_print_int) num_failed
  in
  let pp_time = Util.Format.blue @@ fun out () -> fprintf out "%.2fs" time in
  (* OK: n/n tests passed!*)
  if num_passed = num_tests then
    fprintf
      out
      "%a %a in %a."
      (Util.Format.green pp_print_string)
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
      (Util.Format.green pp_print_string)
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
      (Util.Format.red pp_print_string)
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
      (Util.Format.red pp_print_string)
      fail_label
      pp_passed
      ()
      pp_failed
      ()
      pp_time
      ()

let pp_results out res =
  let open Util.Format in
  let to_row
    { name; num_passed; status; time; is_unit; log = _; verbose_log = _ }
    =
    let status_cell =
      let bracket color icon =
        Table.cell (fun out () -> fprintf out "%a" (color pp_print_string) icon)
      in
      match status with
      | Pass -> bracket green "✓"
      | Fail _ -> bracket red "✖"
      | Discarded _ -> bracket yellow "☐"
    in
    let name =
      let color =
        match status with
        | Fail _ -> red
        | _ -> Util.Format.blue
      in
      Table.cell @@ fun out () ->
      fprintf
        out
        "%a"
        (color pp_print_string)
        (Option.fold ~none:"Anonymous" ~some:Fun.id name)
    in
    let num_passed =
      let msg =
        match status with
        | Pass ->
          if is_unit then
            "Passed"
          else
            Printf.sprintf "Passed %d samples" num_passed
        | Fail { explanation; _ } -> explanation
        | Discarded { num_discarded } ->
          Printf.sprintf "Passed %d and %d discarded" num_passed num_discarded
      in
      Table.cell @@ fun out () -> fprintf out "%a" (faint pp_print_string) msg
    in
    let time =
      Table.cell @@ fun out () ->
      let msg = Printf.sprintf "%.0fms" (time *. 1000.) in
      fprintf out "%a" (blue pp_print_string) msg
    in
    [ status_cell; name; num_passed; time ]
  in
  let columns = [ Table.left; Table.left; Table.left; Table.right ] in
  Table.of_list ~columns @@ List.map to_row res |> Table.pp out

(* Inspired by Alcotest: 
https://github.com/mirage/alcotest/blob/a5d3c0e498a8706427e6337d49a1cbf235b4231d/src/alcotest-engine/pp.ml#L184 *)
let with_box (type a) f out (a : a) =
  let text_width = Util.Format.rendered_length f a + 4 in
  let width =
    let min_box_width = 90 in
    max min_box_width text_width
  in
  let line = List.init (width - 2) (fun _ -> "─") |> String.concat "" in
  let pp_top out () = fprintf out "%s%s%s" "┌" line "┐" in
  let pp_mid out () =
    let pp_bar out () = Util.Format.faint pp_print_string out "│" in
    let space = String.make (width - text_width) ' ' in
    fprintf out "%a %a%s %a" pp_bar () f a space pp_bar ()
  in
  let pp_bottom out () = fprintf out "└%s┘" line in
  fprintf
    out
    "%a@.%a@.%a"
    (Util.Format.faint pp_top)
    ()
    pp_mid
    ()
    (Util.Format.faint pp_bottom)
    ()

let pp_failed_results out res =
  List.iter
    (function
      | { name
        ; num_passed
        ; status =
            Fail
              { explanation = _; num_shrinks; num_attempts = _; location; pp }
        ; time = _
        ; log
        ; verbose_log = _
        ; is_unit
        } ->
        let num_samples = num_passed + 1 in
        let name = Option.fold ~none:"" ~some:(Printf.sprintf "`%s'") name in
        let pp_header out () =
          if is_unit then
            fprintf out "Failed %a" (Util.Format.red pp_print_string) name
          else
            fprintf
              out
              "Failed %a after %a %s and %a shrinks."
              (Util.Format.red pp_print_string)
              (if name = "" then "Anonymous" else name)
              (Util.Format.blue pp_print_int)
              num_samples
              (if num_samples = 1 then "sample" else "samples")
              (Util.Format.blue pp_print_int)
              num_shrinks
        in
        let pp_reason out () =
          Format.fprintf out "@[<v 2>Reason:@;@;%a@]" pp ()
        in
        let pp_log out () =
          if Util.Format.rendered_length Log.pp log > 0 then
            fprintf out "@,@[<v 2>Log:@,@,%a@]" Log.pp log
          else
            ()
        in
        let pp_location out () =
          match location with
          | Some loc ->
            fprintf
              out
              "@[<v 2>Location:@,@,%a@]@;"
              (Util.Format.blue pp_print_string)
              loc
          | None -> ()
        in
        fprintf
          out
          "@[<v 2>%a@[<v 2>@,%a@,%a@,%a@]@,@]"
          (with_box pp_header)
          ()
          pp_reason
          ()
          pp_log
          ()
          pp_location
          ()
      | _ -> ())
    res

let pp_verbose out results =
  results
  |> List.iter (fun { name; verbose_log; _ } ->
       match verbose_log with
       | Some log ->
         let name = Option.fold ~none:"Anonymous" ~some:Fun.id name in
         fprintf
           out
           "@[<v 2>%a@,@,@[<v 2>%a@]@]"
           pp_print_string
           name
           Log.pp
           log
       | None -> ())

let pp out ({ results; _ } as res) =
  if num_tests res = 0 then
    fprintf out "No tests run."
  else
    fprintf
      out
      "@.@[<v 2>%a@]@.@[<v 2>%a@,%a@]@;%a"
      pp_verbose
      results
      pp_header
      res
      pp_results
      results
      pp_failed_results
      results
