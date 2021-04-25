open Popper

type 'a tree =
  | Leaf of 'a
  | Node of 'a tree * 'a tree
[@@deriving show, ord, popper]

let leaf x = Leaf x
let node x y = Node (x, y)

let rec map f = function
  | Leaf x -> Leaf (f x)
  | Node (a, b) -> node (map f a) (map f b)

let rec to_list = function
  | Leaf x -> [ x ]
  | Node (a, b) -> to_list a @ to_list b

type test_data =
  { tree : int tree
  ; f : int -> int
  }
[@@deriving sample]

let test_map =
  let open Syntax in
  test (fun () ->
    let* { tree; f } = sample_test_data in
    let r1 = List.map f @@ to_list tree in
    let r2 = to_list @@ map f tree in
    eq (Comparator.list Comparator.int) r1 r2)

let suite = suite [ ("Map tree", test_map) ]
