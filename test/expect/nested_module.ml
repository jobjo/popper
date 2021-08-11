open Popper
open Sample.Syntax

module X = struct
  module Y = struct
    module Z = struct
      type t = int [@@deriving show, popper, ord]
      type foo = int [@@deriving show, popper, ord]
    end
  end
end

type k =
  { t : X.Y.Z.t
  ; foo : X.Y.Z.foo
  }
[@@deriving show, popper, ord]

let test =
  test @@ fun () ->
  let* x = k_sample in
  equal k_comparator x x

let () = Clean.run test
