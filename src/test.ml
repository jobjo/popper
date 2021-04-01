open Random.Syntax
module Seq = Containers.Seq

type t =
  | Single of Test_result.result Random.t
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

let run ?(seed = Random.Seed.make 42) ts =
  let rec flatten = function
    | Single res -> [ None, res ]
    | Suite ts ->
      List.concat_map
        (fun (name1, test) ->
          List.map
            (fun (name2, res) ->
              let name =
                match name2 with
                | None -> Some name1
                | Some n2 -> Some (Printf.sprintf "%s -> %s" name1 n2)
              in
              name, res)
            (flatten test))
        ts
  in
  let results =
    let+ results =
      flatten ts
      |> List.map (fun (name, res) ->
           Random.map (fun x -> { x with Test_result.name }) res)
      |> Random.sequence
    in
    let num_failed = List.length @@ List.filter is_result_fail results in
    let num_passed = List.length @@ List.filter is_result_passed results in
    let num_discarded =
      List.length @@ List.filter is_result_discarded results
    in
    num_passed, num_failed, num_discarded, results
  in
  let random =
    let+ (num_passed, num_failed, num_discarded, results), time =
      Random.timed results
    in
    { Test_result.num_passed; num_discarded; num_failed; results; time }
  in
  Random.eval seed random |> Test_result.pp Format.std_formatter

let single t = Single t
let suite ts = Suite ts
let max_count_shrinks = 1000
let max_count_find_next = 100
let max_count_discarded = 1000

let test ?(count = 200) f =
  let eval () =
    let* inputs = Input.make_seq in
    let rec aux ~num_discarded ~num_passed outputs =
      if num_passed >= count then
        Random.return (num_passed, Test_result.Pass, Log.empty)
      else if num_discarded > max_count_discarded then
        Random.return
          (num_passed, Test_result.Discarded { num_discarded }, Log.empty)
      else
        let output = Seq.head_exn outputs in
        let next () = Seq.tail_exn outputs () in
        match Output.value output with
        | Proposition.Pass ->
          aux ~num_discarded ~num_passed:(num_passed + 1) next
        | Proposition.Discard ->
          aux ~num_discarded:(num_discarded + 1) ~num_passed next
        | Proposition.Fail { pp; location } ->
          let* res =
            Shrink.shrink ~max_count_find_next ~max_count_shrinks output @@ f ()
          in
          let explanation =
            Printf.sprintf "Failed after %d samples" num_passed
          in
          (match res with
          | Some (num_shrinks, pp, output) ->
            Random.return
              ( num_passed
              , Test_result.Fail { num_shrinks; explanation; pp; location }
              , Output.log output )
          | None ->
            Random.return
              ( num_passed
              , Test_result.Fail { num_shrinks = 0; explanation; pp; location }
              , Output.log output ))
    in
    aux
      ~num_discarded:0
      ~num_passed:0
      (Seq.map (fun x -> Generator.run x @@ f ()) inputs)
  in
  let test =
    let+ (num_passed, status, log), time =
      Random.timed @@ Random.delayed eval
    in
    { Test_result.name = None; num_passed; status; time; log }
  in
  single test

let equal ?loc testable x y =
  Generator.return @@ Proposition.equal ?loc testable x y

let is_true ?loc b = Generator.return @@ Proposition.is_true ?loc b
