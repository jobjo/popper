module Format = struct
  let style_regexp = Str.regexp "\\[[0-9]+m"

  let rendered_length : type a. (Format.formatter -> a -> unit) -> a -> int =
   fun f a ->
    Format.kasprintf Fun.id "%a" f a
    |> Str.global_replace style_regexp ""
    |> String.to_seq
    |> Seq.filter (fun c -> Char.code c <> 27)
    |> Seq.fold_left (fun n _ -> n + 1) 0
end
