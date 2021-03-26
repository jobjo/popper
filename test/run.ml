module Seq = Containers.Seq
open Popper
open Generator

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
  let open Generator in
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

let fun_gen : (int -> int) Generator.t = Generator.arrow Generator.int

let my_gen =
  let open Syntax in
  let* f = fun_gen in
  let* g = fun_gen in
  let* h = fun_gen in
  return (f, g, h)

let print_input output =
  Popper.Output.consumed output
  |> List.concat_map Popper.Consumed.data
  |> List.map (fun n -> Printf.sprintf "%d" @@ Int32.to_int n)
  |> String.concat "; "
  |> Printf.printf "[%s]\n"

type series = {
  data : string list;
  numbers : int list;
  name : string;
  range : float * float;
}
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
  let* x = float in
  let* y = float in
  let range = (min x y, max x y +. 2.) in
  let series = { data; numbers; name; range } in
  return (Popper.Proposition.equals pp_series (rev @@ rev series) series)

let () =
  let () = print_endline "Testing" in
  let seed = Popper.Random.Seed.make_self_init () in
  match Popper.Random.eval seed @@ Test.test ~count:10_000 test_rev_twice with
  | Ok () -> print_endline "passed"
  | Error pp -> pp Format.std_formatter ()
