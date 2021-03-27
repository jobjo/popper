type color =
  | Red
  | Green

let red = Red
let green = Green

let style_code = function
  | Some Red -> "31"
  | Some Green -> "32"
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
