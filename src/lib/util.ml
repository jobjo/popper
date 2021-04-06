module Format = struct
  let style_regexp = Str.regexp "\\[[0-9]+m"

  let rendered_length : type a. (Format.formatter -> a -> unit) -> a -> int =
   fun f a ->
    Format.kasprintf Fun.id "%a" f a
    |> Str.global_replace style_regexp ""
    |> String.to_seq
    |> Seq.filter (fun c -> Char.code c <> 27)
    |> Seq.fold_left (fun n _ -> n + 1) 0

  type color =
    | Red
    | Green
    | Yellow
    | Blue
    | Faint

  let red = Red
  let green = Green
  let blue = Blue
  let yellow = Yellow
  let faint = Faint

  let style_code = function
    | Some Red -> "31"
    | Some Green -> "32"
    | Some Yellow -> "33"
    | Some Blue -> "36"
    | Some Faint -> "2"
    | None -> "0"

  let pp_color color pp out v =
    let set color =
      Format.pp_print_as out 0 "\027[";
      Format.pp_print_as out 0 (style_code color);
      Format.pp_print_as out 0 "m"
    in
    set @@ Some color;
    pp out v;
    set None

  let green pp = pp_color green pp
  let yellow pp = pp_color yellow pp
  let red pp = pp_color red pp
  let blue pp = pp_color blue pp
  let faint pp = pp_color faint pp
end

module Seq = struct
  let head_tail_exn s =
    match s () with
    | Seq.Nil -> failwith "Unexpected"
    | Cons (a, f) -> a, f
end
