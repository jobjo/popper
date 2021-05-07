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

module List = struct
  let head_opt = function
    | [] -> None
    | x :: _ -> Some x

  let concat_map f l =
    let rec aux f acc = function
      | [] -> List.rev acc
      | x :: l ->
        let xs = f x in
        aux f (List.rev_append xs acc) l
    in
    aux f [] l
end

module Seq = struct
  let head_tail_exn s =
    match s () with
    | Seq.Nil -> failwith "Unexpected"
    | Cons (a, f) -> (a, f)

  let rec take n xs () =
    if n = 0 then
      Seq.Nil
    else
      match xs () with
      | Seq.Nil -> Nil
      | Cons (x, xs) -> Cons (x, take (n - 1) xs)

  let rec append xs ys () =
    match xs () with
    | Seq.Nil -> ys ()
    | Cons (x, next) -> Seq.Cons (x, append next ys)

  let rec drop n xs () =
    match xs () with
    | xs when n = 0 -> xs
    | Seq.Nil -> Nil
    | Cons (_, xs) -> drop (n - 1) xs ()

  let rec unfold f u () =
    match f u with
    | None -> Seq.Nil
    | Some (x, u') -> Cons (x, unfold f u')
end

module Timer = struct
  let time_it f =
    let start = Unix.gettimeofday () in
    let res = f () in
    let stop = Unix.gettimeofday () in
    (res, stop -. start)
end
