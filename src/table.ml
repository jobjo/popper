module IM = Map.Make (Int)

module PM = Map.Make (struct
  type t = int * int

  let compare = compare
end)

type alignment =
  | Left
  | Center
  | Right

type cell =
  { color : Printer.color option
  ; value : string
  }

let left = Left
let right = Right
let center = Center
let text ?color value = { color; value }

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
      ~some:(fun { value; _ } -> 4 + String.length value)
      (cell ~row ~col))
  |> List.fold_left max 0

let make_space n = String.concat "" @@ List.init n (Fun.const " ")

let render_cell ~column_width { color; value } align out =
  let cell_width = String.length value in
  let space = column_width - cell_width in
  let cell_str =
    match align with
    | Left -> Printf.sprintf "%s%s" value (make_space space)
    | Center ->
      let space_left = space / 2 in
      let space_right = (space / 2) + (space mod 2) in
      Printf.sprintf
        "%s%s%s"
        (make_space space_left)
        value
        (make_space space_right)
    | Right -> Printf.sprintf "%s%s" (make_space space) value
  in
  match color with
  | Some color -> Printer.pp_color color Format.pp_print_string out cell_str
  | None -> Format.fprintf out "%s" cell_str

let of_list ~columns rows =
  let cell =
    let map =
      List.mapi (fun row -> List.mapi (fun col cell -> (row, col), cell)) rows
      |> List.concat
      |> List.to_seq
      |> PM.of_seq
    in
    fun ~row ~col -> PM.find_opt (row, col) map
  in
  let num_rows = List.length rows in
  let num_cols =
    Option.fold ~none:0 ~some:List.length (Containers.List.head_opt rows)
  in
  { cell; num_rows; num_cols; columns }

let pp out { num_rows; num_cols; cell; columns } =
  let col_widths = List.init num_cols (max_column_length ~num_rows ~cell) in
  let cols_and_widths = List.combine columns col_widths in
  let render_cell ~row ~col ~column_width column =
    let cell =
      Option.fold ~none:{ color = None; value = "" } ~some:Fun.id
      @@ cell ~row ~col
    in
    render_cell ~column_width cell column out
  in
  let render_row row =
    List.iteri
      (fun col (column, column_width) ->
        render_cell ~row ~col ~column_width column)
      cols_and_widths;
    Format.fprintf out "@;"
  in
  List.iter render_row (List.init num_rows Fun.id)
