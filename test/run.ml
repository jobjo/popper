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

let print_output { consumed; value; _ } =
  Printf.printf "[%s]"
  @@ String.concat ";\n"
  @@ List.map Stream.show_consumed consumed;
  print_endline @@ show value

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

let test ~seed ~count prop =
  let inputs = Input.make_seq seed in
  let test input = run input prop in
  let rec aux num_passed outputs =
    if num_passed >= count then Result.Ok ()
    else
      let { value; _ } = Seq.head_exn outputs in
      let next = Seq.tail_exn outputs in
      match value with
      | Proposition.Pass -> aux (num_passed + 1) next
      | Proposition.Discard -> aux num_passed next
      | Proposition.Fail pp ->
        let pp out () =
          Format.fprintf out "@[<v 2>Failed after %d attempts:@,%a@]"
            num_passed pp ()
        in
        Result.Error pp
  in
  aux 0 (Seq.map test inputs)

type series = { data : int list } [@@deriving show]

let rev { data } =
  match data with
  | [ x; y; z; h ] -> { data = [ h; z; x; y ] }
  | _ -> { data = List.rev data }

let test_rev_twice =
  let open Syntax in
  let* data = many int in
  let series = { data } in
  return (Proposition.equals pp_series (rev @@ rev series) series)

let () =
  match test ~seed:42 ~count:10 test_rev_twice with
  | Ok () -> print_endline "Passed"
  | Error pp -> pp Format.std_formatter ()
