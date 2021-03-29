module Seq = Containers.Seq
open Popper
open Generator

type t =
  { bool : bool
  ; name : string
  ; numbers : int list
  ; number : int
  ; float : float
  ; fn : int -> bool
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

type series =
  { data : string list
  ; numbers : int list
  ; name : string
  ; range : int * int
  }
[@@deriving show]

let rev ({ data; _ } as series) =
  match data with
  | [ x; y; z; _; _; _; _; _ ] -> { series with data = [ z; x; y ] }
  | _ -> { series with data = List.rev data }

type exp =
  | Lit of bool
  | And of exp * exp
  | Or of exp * exp
  | Not of exp
[@@deriving show]

let rec eval = function
  | Lit b -> b
  | And (e1, e2) -> eval e1 || eval e2
  | Or (e1, e2) -> eval e1 || eval e2
  | Not b -> not @@ eval b

let gen_exp =
  let open Syntax in
  let rec aux size () =
    let lit = one_value_of [ Lit true; Lit false ] in
    let and_ () =
      let* e1 = delayed (aux (size * 2)) in
      let* e2 = delayed (aux (size * 2)) in
      return @@ And (e1, e2)
    in
    let or_ () =
      let* e1 = delayed (aux (size * 2)) in
      let* e2 = delayed (aux (size * 2)) in
      return @@ And (e1, e2)
    in
    let not_ () = map (fun x -> Not x) (delayed (aux (size + 1))) in
    if size > 5 then
      lit
    else
      one_of [ lit; delayed and_; delayed or_; delayed not_ ]
  in
  delayed (aux 1)

let test_exp_and =
  let open Proposition in
  let open Syntax in
  Test.test
    (let+ e1 = gen_exp
     and+ e2 = gen_exp in
     log_input
       (fun out () -> Format.pp_print_list pp_exp out [ e1; e2 ])
       (equals Format.pp_print_bool (eval e1 && eval e2) (eval (And (e1, e2)))))

let test_rev_twice =
  let open Proposition in
  let open Syntax in
  Test.test
    ~count:1000
    (let* data = many string in
     let* numbers = many int in
     let* name = string in
     let* mn = range 0 100 in
     let* mx = range (mn + 10) (mn + 20) in
     let range = mn, mx in
     let series = { data; numbers; name; range } in
     return (equals pp_series (rev @@ rev series) series))

let dummy = Test.unit @@ fun () -> Proposition.pass

let sub_suite =
  Test.suite
    [ "Dummy test", dummy
    ; "AB", dummy
    ; "Bsdfasdasdf", Test.unit (fun () -> Proposition.Discard)
    ]

let suite =
  Test.suite
    [ "Asdfads", dummy
    ; "Reverse twice", test_rev_twice
    ; "Exp and", test_exp_and
    ; "Sub suite", sub_suite
    ]

(* Fook *)
let () = Test.run suite

(* let () =
  let open Table in
  Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ();
  let tbl =
    of_list
      ~columns:[ left; center; right ]
      [ [ text "A1"; text "A2"; text ~color:Printer.green "A3" ]
      ; [ text "A1"; text "A2"; text "A3" ]
      ; [ text ~color:Printer.red "Gurka i Magen"
        ; text "Slugger"
        ; text "Markaryd"
        ]
      ; [ text "Sats"; text "Mokus"; text "332.3" ]
      ]
  in
  Format.fprintf Format.std_formatter "@[<v 2>@,%a@]" pp tbl *)
