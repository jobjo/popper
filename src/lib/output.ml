type 'a t =
  { value : 'a
  ; consumed : Consumed.t
  ; remaining : Input.t
  ; log : Log.t
  ; size : int
  }

let make ~value ~size ~consumed ~remaining ~log =
  { size; value; consumed; remaining; log }

let value { value; _ } = value
let size { size; _ } = size
let set_size size v = { v with size }
let consumed { consumed; _ } = consumed
let set_consumed consumed output = { output with consumed }
let set_value value output = { output with value }
let set_remaining remaining output = { output with remaining }
let remaining { remaining; _ } = remaining
let log { log; _ } = log
let set_log log output = { output with log }

let map f { size; value; consumed; remaining; log } =
  { value = f value; size; consumed; remaining; log }

let tag tag { value; size; consumed; remaining; log } =
  let consumed = Consumed.tag tag consumed in
  { value; size; consumed; remaining; log }
