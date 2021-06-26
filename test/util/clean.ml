let time_part_regexp = Str.regexp "[0-9]+ms"
let time_total_regexp = Str.regexp "[0-9]+.[0-9]+s"

let replace ~postfix s =
  let matched = Str.matched_string s in
  let blank =
    List.init (String.length matched - String.length postfix) (fun _ -> ' ')
    |> List.to_seq
    |> String.of_seq
  in
  Printf.sprintf "%s%s" blank postfix

let blank_time s =
  s
  |> Str.global_substitute time_total_regexp (fun _ -> "XXs")
  |> Str.global_substitute time_part_regexp (fun _ -> "")

let style_regexp = Str.regexp "\\[[0-9]+m"

let strip_control_codes s =
  s
  |> Str.global_replace style_regexp ""
  |> String.to_seq
  |> Stdlib.Seq.filter (fun c -> Char.code c <> 27)
  |> String.of_seq

let leading_spaces s =
  let s = Printf.sprintf "%s." s in
  let s' = String.trim s in
  String.length s - String.length s'

let trim f xs =
  let rec aux = function
    | [] -> []
    | x :: xs when f x -> aux xs
    | xs -> xs
  in
  aux xs |> List.rev |> aux |> List.rev

let normalize s =
  let lines =
    s
    |> strip_control_codes
    |> String.split_on_char '\n'
    |> List.map (fun line ->
         let s = Printf.sprintf "|%s" (blank_time line) in
         let s = String.trim s in
         let s = String.sub s 1 (String.length s - 1) in
         s)
    |> trim (fun s -> String.trim s = "")
  in
  let min_leading_spaces =
    let accum ms s =
      let n = leading_spaces s in
      min ms n
    in
    match lines with
    | [] -> 0
    | _ -> List.fold_left accum Int.max_int lines
  in
  lines
  |> List.map (fun s ->
       String.sub s min_leading_spaces (String.length s - min_leading_spaces))
  |> String.concat "\n"

let run suite =
  let config = Popper.Config.formatter Format.str_formatter in
  let () =
    try Popper.run ~config suite with
    | _ -> ()
  in
  let output = Format.flush_str_formatter () in
  Printf.printf "%s\n" @@ normalize output
