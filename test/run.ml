module Seq = Containers.Seq
open Popper
open Popper.Generator

type t = {
  bool : bool;
  name : string;
  numbers : int list;
  number : int;
  float : float;
  fn : int -> bool;
}
[@@deriving show]

let gen =
  let open Syntax in
  let* name = string in
  let* numbers = many int in
  let* number = one_value_of [ 1; 2; 3 ] in
  let* float = float in
  let* fn = arrow bool in
  let* bool = bool in
  return { bool; name; numbers; number; float; fn }

let print_list xs =
  Printf.printf "[";
  List.iter (fun i -> Printf.printf "%d; " (Int32.to_int i)) xs;
  Printf.printf "]\n"

let fun_gen : (int -> int) gen = arrow int

let my_gen =
  let open Syntax in
  let* f = fun_gen in
  let* g = fun_gen in
  let* h = fun_gen in
  return (f, g, h)

let find_next prop output =
  let open Random.Syntax in
  let rec aux ix =
    if ix > 100 then Random.return None
    else
      let* output =
        let+ inputs = Shrink.shrink output in
        inputs
        |> Seq.take 1000
        |> Seq.filter_map (fun input ->
             let output = Generator.run input prop in
             if Proposition.is_fail @@ Output.value output then Some output
             else None)
        |> Seq.head
      in
      match output with
      | Some output -> Random.return (Some output)
      | None -> aux (ix + 1)
  in
  aux 0

let print_input output =
  Output.consumed output
  |> List.concat_map Consumed.data
  |> List.map (fun n -> Printf.sprintf "%d" @@ Int32.to_int n)
  |> String.concat "; "
  |> Printf.printf "[%s]\n"

let shrink ~max_count output prop =
  let open Random.Syntax in
  let rec aux ix output =
    print_input output;
    if ix >= max_count then
      match Output.value output with
      | Proposition.Fail pp -> Random.return @@ Some (ix, pp)
      | _ -> Random.return None
    else
      match Output.value output with
      | Proposition.Fail _ -> (
        let* output = find_next prop output in
        match output with
        | Some output -> aux (ix + 1) output
        | None -> Random.return None)
      | _ -> Random.return None
  in
  aux 0 output

let test ~count prop =
  let open Random.Syntax in
  let* seed = Random.seed in
  let inputs = Input.make_seq seed in
  let test input = run input prop in
  let rec aux num_passed outputs =
    if num_passed >= count then Random.return @@ Result.Ok ()
    else
      let output = Seq.head_exn outputs in
      let next = Seq.tail_exn outputs in
      match Output.value output with
      | Proposition.Pass -> aux (num_passed + 1) next
      | Proposition.Discard -> aux num_passed next
      | Proposition.Fail pp ->
        let* num_shrinks, pp =
          let+ res = shrink ~max_count:10_000 output prop in
          Containers.Option.get_or ~default:(0, pp) res
        in
        let pp out () =
          Format.fprintf out
            "@[<v 2>Failed after %d attempts and %d shrinks:@,%a@]" num_passed
            num_shrinks pp ()
        in
        Random.return (Result.Error pp)
  in
  aux 0 (Seq.map test inputs)

type series = { data : string list; numbers : int list; name : string }
[@@deriving show]

let rev ({ data; _ } as foo) =
  match data with
  | [ x; y; z; h; _; _; _; _ ] -> { foo with data = [ h; z; x; y ] }
  | _ -> { foo with data = List.rev data }

let test_rev_twice =
  let open Syntax in
  let* data = many string in
  let* numbers = many int in
  let* name = string in
  let series = { data; numbers; name } in
  return (Proposition.equals pp_series (rev @@ rev series) series)

let () =
  let () = print_endline "Testing" in
  let seed = Random.make_seed_self_init () in
  match Random.eval seed @@ test ~count:10_000 test_rev_twice with
  | Ok () -> print_endline "passed"
  | Error pp -> pp Format.std_formatter ()
