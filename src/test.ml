open Random.Syntax
module Seq = Containers.Seq

type error =
  { num_shrinks : int
  ; pp : Format.formatter -> unit -> unit
  }

type result =
  { num_samples : int
  ; error : error option
  }

type t =
  | Single of result Random.t
  | Suite of (string * t) list

let rec flatten = function
  | Single res -> [ None, res ]
  | Suite ts ->
    List.concat_map
      (fun (name1, test) ->
        List.map
          (fun (name2, res) ->
            let name =
              match name2 with
              | None -> Some name1
              | Some n2 -> Some (Printf.sprintf "%s -> %s" name1 n2)
            in
            name, res)
          (flatten test))
      ts

let run ~seed ts =
  let random =
    flatten ts
    |> List.map (fun (name, res) -> Random.map (fun x -> name, x) res)
    |> Random.sequence
  in
  Random.eval seed random

let single t = Single t
let suite ts = Suite ts
let max_count_shrinks = 10_000
let max_count_find_next = 1000

let test ?(count = 200) prop =
  let result =
    let* inputs = Input.make_seq in
    let test input = Generator.run input prop in
    let rec aux num_samples outputs =
      if num_samples >= count then
        Random.return { num_samples; error = None }
      else
        let output = Seq.head_exn outputs in
        let next = Seq.tail_exn outputs in
        match Output.value output with
        | Proposition.Pass -> aux (num_samples + 1) next
        | Proposition.Discard -> aux num_samples next
        | Proposition.Fail pp ->
          let* num_shrinks, pp =
            let+ res =
              Shrink.shrink ~max_count_find_next ~max_count_shrinks output prop
            in
            Containers.Option.get_or ~default:(0, pp) res
          in
          Random.return { num_samples; error = Some { num_shrinks; pp } }
    in
    aux 0 (Seq.map test inputs)
  in
  single result
