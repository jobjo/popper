module Seq = Containers.Seq

type success = { num_runs : int }

type error =
  { num_runs : int
  ; num_shrinks : int
  ; pp : Format.formatter -> unit -> unit
  }

type result = (success, error) Result.t

let max_count_shrinks = 10_000
let max_count_find_next = 1000

let test ?(count = 200) prop =
  let open Random.Syntax in
  let* inputs = Input.make_seq in
  let test input = Generator.run input prop in
  let rec aux num_passed outputs =
    if num_passed >= count then
      Random.return @@ Result.Ok ()
    else (
      let output = Seq.head_exn outputs in
      let next = Seq.tail_exn outputs in
      match Output.value output with
      | Proposition.Pass -> aux (num_passed + 1) next
      | Proposition.Discard -> aux num_passed next
      | Proposition.Fail pp ->
        let* num_shrinks, pp =
          let+ res =
            Shrink.shrink ~max_count_find_next ~max_count_shrinks output prop
          in
          Containers.Option.get_or ~default:(0, pp) res
        in
        let pp out () =
          Format.fprintf
            out
            "@[<v 2>Failed after %d attempts and %d shrinks:@,%a@]"
            num_passed
            num_shrinks
            pp
            ()
        in
        Random.return (Result.Error pp))
  in
  aux 0 (Seq.map test inputs)
