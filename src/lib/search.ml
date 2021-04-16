open Random.Syntax

module type Config = sig
  type t

  val max_tries : int
  val compare : t -> t -> int
  val keep : t -> t option
  val modify : t -> t Random.t
end

type 'a result =
  { num_attempts : int
  ; num_explored : int
  ; node : 'a
  }

module type S = sig
  type t

  val search : t -> t result Random.t
end

module Make (C : Config) = struct
  type t = C.t

  module S = Set.Make (C)

  let num_attempts = ref 0
  let visited = ref S.empty

  let reset () =
    visited := S.empty;
    num_attempts := 0

  let visit n = visited := S.add n !visited
  let is_visited n = S.mem n !visited
  let keep node = C.keep node

  let find_next node =
    let rec aux ix =
      if ix >= C.max_tries then
        Random.return None
      else
        let* node = C.modify node in
        let () = incr num_attempts in
        if is_visited node then
          aux (ix + 1)
        else
          let () = visit node in
          match keep node with
          | Some node -> Random.return (Some node)
          | None -> aux (ix + 1)
    in
    aux 0

  let search node =
    reset ();
    let rec aux ix node =
      let* no = find_next node in
      match no with
      | Some n -> aux (ix + 1) n
      | None -> Random.return (node, ix)
    in
    let+ node, num_explored = aux 0 node in
    { num_attempts = !num_attempts; num_explored; node }
end
