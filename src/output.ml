type 'a t =
  { value : 'a
  ; consumed : Consumed.t list
  ; remaining : Input.t
  }

let make ~value ~consumed ~remaining = { value; consumed; remaining }
let value { value; _ } = value
let consumed { consumed; _ } = consumed
let set_consumed consumed output = { output with consumed }
let set_value value output = { output with value }
let set_remaining remaining output = { output with remaining }
let remaining { remaining; _ } = remaining

let map f { value; consumed; remaining } =
  { value = f value; consumed; remaining }

let tag tag { value; consumed; remaining } =
  let data = List.concat_map Consumed.data consumed in
  let consumed = [ Consumed.make tag data ] in
  { value; consumed; remaining }

(* let data_of_output { consumed; _ } =
  consumed |> List.concat_map (fun { data; _ } -> data) |> List.to_seq *)
