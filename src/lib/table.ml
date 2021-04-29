module IM = Map.Make (Int)

module PM = Map.Make (struct
  type t = int * int

  let compare = compare
end)

type alignment =
  | Left
  | Center
  | Right

type cell = Format.formatter -> unit -> unit

let left = Left
let right = Right
let center = Center

(* let text ?color value = { color; value = (fun out () -> Format.fprintf out
   "%s" value) } *)

let cell = Fun.id

type t =
  { num_rows : int
  ; num_cols : int
  ; cell : row:int -> col:int -> cell option
  ; columns : alignment list
  }

let max_column_length ~num_rows ~cell col =
  List.init num_rows (fun row ->
    Option.fold
      ~none:0
      ~some:(fun value -> Util.Format.rendered_length value ())
      (cell ~row ~col))
  |> List.fold_left max 0

let make_space n = String.concat "" @@ List.init n (Fun.const " ")

let render_cell ~column_width cell align out =
  let cell_width = Util.Format.rendered_length cell () in
  let space = column_width - cell_width in
  let margin = make_space 2 in
  match align with
  | Left -> Format.fprintf out "%a%s%s" cell () (make_space space) margin
  | Center ->
    let space_left = space / 2 in
    let space_right = (space / 2) + (space mod 2) in
    Format.fprintf
      out
      "%s%a%s%s"
      (make_space space_left)
      cell
      ()
      (make_space space_right)
      margin
  | Right -> Format.fprintf out "%s%a%s" (make_space space) cell () margin

let of_list ~columns rows =
  let cell =
    let map =
      List.mapi (fun row -> List.mapi (fun col cell -> ((row, col), cell))) rows
      |> List.concat
      |> List.to_seq
      |> PM.of_seq
    in
    fun ~row ~col -> PM.find_opt (row, col) map
  in
  let num_rows = List.length rows in
  let num_cols =
    Option.fold ~none:0 ~some:List.length (Util.List.head_opt rows)
  in
  { cell; num_rows; num_cols; columns }

let pp out { num_rows; num_cols; cell; columns } =
  let open Format in
  if num_rows = 0 then
    ()
  else
    let col_widths = List.init num_cols (max_column_length ~num_rows ~cell) in
    let cols_and_widths = List.combine columns col_widths in
    let pp_cell ~row ~col ~column_width out alignment =
      let cell =
        Option.fold ~none:(fun _ _ -> ()) ~some:Fun.id @@ cell ~row ~col
      in
      render_cell ~column_width cell alignment out
    in
    let render_row out row =
      List.iteri
        (fun col (column, column_width) ->
          pp_cell ~row ~col ~column_width out column)
        cols_and_widths;
      pp_print_cut out ()
    in
    let pp_rows out () =
      List.iter (render_row out) (List.init num_rows Fun.id)
    in
    fprintf out "@[<v 0>@,%a@]" pp_rows ()
