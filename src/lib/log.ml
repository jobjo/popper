(* type t = | Empty | Print of (Format.formatter -> unit -> unit) | Add of t * t *)

type t = Format.formatter -> unit -> unit

let empty _ _ = ()

let of_pp (pp : Format.formatter -> unit) out () =
  let pp out () = pp out in
  Format.fprintf out "@[<v>%a@,@]" pp ()

let pp out (t : t) = t out ()
let add p1 p2 out () = Format.fprintf out "%a%a" pp p1 pp p2
