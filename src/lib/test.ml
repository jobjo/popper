open Random.Syntax

type t =
  | Single of (Config.t -> Test_result.result Random.t)
  | Suite of (string * t) list

let is_result_passed { Test_result.status; _ } =
  match status with
  | Pass -> true
  | _ -> false

let is_result_fail { Test_result.status; _ } =
  match status with
  | Fail _ -> true
  | _ -> false

let is_result_discarded { Test_result.status; _ } =
  match status with
  | Discarded _ -> true
  | _ -> false

let run ?(config = Config.default) ts =
  let seed = Random.Seed.make @@ Config.get_seed config in
  let rec flatten = function
    | Single res -> [ (None, res) ]
    | Suite ts ->
      Util.List.concat_map
        (fun (name1, test) ->
          List.map
            (fun (name2, res) ->
              let name =
                match name2 with
                | None -> Some name1
                | Some n2 -> Some (Printf.sprintf "%s -> %s" name1 n2)
              in
              (name, res))
            (flatten test))
        ts
  in
  let num_passed, num_failed, num_discarded, results, time =
    let results, time =
      Util.Timer.time_it @@ fun () ->
      flatten ts
      |> List.map (fun (name, f) ->
           let res = f config in
           Random.map (fun x -> { x with Test_result.name }) res
           |> Random.eval seed)
    in
    let num_failed = List.length @@ List.filter is_result_fail results in
    let num_passed = List.length @@ List.filter is_result_passed results in
    let num_discarded =
      List.length @@ List.filter is_result_discarded results
    in
    (num_passed, num_failed, num_discarded, results, time)
  in
  let test_result =
    { Test_result.num_passed; num_discarded; num_failed; results; time }
  in
  let () = Test_result.pp config (Config.get_formatter config) test_result in
  test_result.Test_result.num_failed = 0

let single t = Single t
let suite ts = Suite ts

let log_verbose ~index output out =
  let log = Output.log output in
  Format.fprintf
    out
    "@[<v 2>%a@,@,%a@]@,"
    (Util.Format.faint Format.pp_print_string)
    (Printf.sprintf "Sample %d:" (index + 1))
    Log.pp
    log

let make ?(config = Config.default) test_fun =
  let test_fun () =
    try test_fun () with
    | e -> Sample.return @@ Proposition.fail_with (Printexc.to_string e)
  in
  let max_num_discarded = Config.get_max_num_discarded config in
  let eval override =
    let config = Config.all [ override; config ] in
    let count = Config.get_num_samples config in
    let verbose_log =
      if Config.get_verbose config then Some Log.empty else None
    in
    let size = Config.get_max_size config in
    let max_length = Config.get_max_input_length config in
    let max_tries = Config.get_max_shrinks config in
    let max_tries_modify = Config.get_max_shrink_modify_attempts config in
    let* inputs = Input.make_seq ~max_length ~size in
    let rec aux ~num_discarded ~num_passed ~verbose_log outputs =
      if num_passed >= count then
        Random.return
          (num_passed, Test_result.Pass, Log.empty, verbose_log, false)
      else if num_discarded > max_num_discarded then
        let pp out () =
          Format.fprintf
            out
            {|Maximum number of discarded samples exceeded. The configured limit is %d.|}
            max_num_discarded
        in
        Random.return
          ( num_passed
          , Test_result.Fail
              { num_shrinks = 0
              ; num_attempts = 1
              ; explanation = "Too many samples discarded"
              ; pp
              ; location = None
              }
          , Log.empty
          , verbose_log
          , false )
      else
        let output, next = Util.Seq.head_tail_exn outputs in
        let verbose_log =
          verbose_log
          |> Option.map (fun log ->
               Log.add log (Log.of_pp @@ log_verbose ~index:num_passed output))
        in
        let is_unit = Consumed.is_empty @@ Output.consumed output in
        match Output.value output with
        | Proposition.Pass ->
          if is_unit then
            Random.return (1, Test_result.Pass, Log.empty, verbose_log, is_unit)
          else
            aux ~num_discarded ~num_passed:(num_passed + 1) next ~verbose_log
        | Proposition.Discard ->
          if is_unit then
            Random.return
              ( 0
              , Test_result.Discarded { num_discarded = 1 }
              , Log.empty
              , verbose_log
              , is_unit )
          else
            aux ~num_discarded:(num_discarded + 1) ~num_passed next ~verbose_log
        | Proposition.Fail { location; pp } ->
          if is_unit then
            Random.return
              ( num_passed
              , Test_result.Fail
                  { num_shrinks = 1
                  ; num_attempts = 1
                  ; explanation = "Failed"
                  ; pp
                  ; location
                  }
              , Output.log output
              , verbose_log
              , is_unit )
          else
            let* { Shrink.num_shrinks; num_attempts; pp; output } =
              Shrink.shrink
                ~max_tries
                ~max_tries_modify
                ~num_shrink_rounds:10
                ~size
                output
                (test_fun ())
            in
            let explanation =
              if is_unit then
                "Failed"
              else
                Printf.sprintf
                  "Failed after %d %s"
                  (num_passed + 1)
                  (if num_passed = 0 then "sample" else "samples")
            in
            Random.return
              ( num_passed
              , Test_result.Fail
                  { num_shrinks; num_attempts; explanation; pp; location }
              , Output.log output
              , verbose_log
              , is_unit )
    in
    aux
      ~num_discarded:0
      ~num_passed:0
      ~verbose_log
      (Seq.map (fun x -> Sample.run x @@ test_fun ()) inputs)
  in
  let test config =
    let+ (num_passed, status, log, verbose_log, is_unit), time =
      Random.timed @@ Random.delayed (fun () -> eval config)
    in
    { Test_result.name = None
    ; num_passed
    ; status
    ; time
    ; log
    ; verbose_log
    ; is_unit
    }
  in
  single test
