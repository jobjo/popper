(* open Alcotest *)

(* let test_lowercase () = check string "same string" "hello!" "hello!"

(* Run it *)
let () =
  run "Utils"
    [ ("string-case", [ test_case "Lower case" `Quick test_lowercase ]) ] *)

open Popper

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

let not_now _ =
  (* Baseline *)
  let input =
    List.to_seq @@ List.map Int32.of_int [ 1093223; 2; 3; 4; 5; 6; 7; 8; 9 ]
  in
  (* Next *)
  let { value = f, g, h; _ } = run input my_gen in
  Printf.printf "f 1: %d\n" @@ f 1;
  Printf.printf "g 1: %d\n" @@ g 1;
  Printf.printf "h 1: %d\n" @@ h 1;
  print_endline "=======================\n\n"

let () = Proposition.main ()
