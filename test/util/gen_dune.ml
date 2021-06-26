(* An executable that generates dune rules for expect tests *)
let gen_rule name =
  Printf.printf
    {|
; Expect test for `%s`
(executable
 (name %s)
 (modules %s)
 (libraries clean)
 (preprocess
  (pps ppx_deriving.show ppx_deriving.ord ppx_deriving_popper)))

(rule
 (with-stdout-to
  %s.output
  (run ./%s.exe)))

(rule
 (alias runtest)
 (action
  (diff %s.expected %s.output)))
|}
    name
    name
    name
    name
    name
    name
    name

let () =
  let files =
    Sys.readdir "."
    |> Array.to_list
    |> List.sort String.compare
    |> List.filter (fun n ->
         (not (Filename.check_suffix n ".pp.ml"))
         && Filename.check_suffix n ".ml")
  in
  match files with
  | [] -> ()
  | tests ->
    print_endline "; This file is generated";
    List.iter
      (fun test ->
        match Filename.chop_suffix_opt ~suffix:".ml" test with
        | Some n -> gen_rule n
        | None -> ())
      tests
