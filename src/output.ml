type 'a t =
  { value : 'a
  ; consumed : Consumed.t list
  ; remaining : Input.t
  ; logs : string list
  }

let make ~value ~consumed ~remaining ~logs =
  { value; consumed; remaining; logs }

let value { value; _ } = value
let consumed { consumed; _ } = consumed
let set_consumed consumed output = { output with consumed }
let set_value value output = { output with value }
let set_remaining remaining output = { output with remaining }
let remaining { remaining; _ } = remaining
let logs { logs; _ } = logs
let set_logs logs output = { output with logs }

let map f { value; consumed; remaining; logs } =
  { value = f value; consumed; remaining; logs }

let tag tag { value; consumed; remaining; logs } =
  let data = List.concat_map Consumed.data consumed in
  let consumed = [ Consumed.make tag data ] in
  { value; consumed; remaining; logs }
