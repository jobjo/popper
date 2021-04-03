module Color = struct
  type t =
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
end

let pp_color color pp out v =
  let set color =
    Format.pp_print_as out 0 "\027[";
    Format.pp_print_as out 0 (Color.style_code color);
    Format.pp_print_as out 0 "m"
  in
  set @@ Some color;
  pp out v;
  set None

let green pp = pp_color Color.green pp
let yellow pp = pp_color Color.yellow pp
let red pp = pp_color Color.red pp
let blue pp = pp_color Color.blue pp
let faint pp = pp_color Color.faint pp
