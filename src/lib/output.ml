type 'a t =
  { value : 'a
  ; consumed : Consumed.t
  ; remaining : Input.t
  ; log : Log.t
  }

let make ~value ~consumed ~remaining ~log = { value; consumed; remaining; log }
let value { value; _ } = value
let consumed { consumed; _ } = consumed
let set_consumed consumed output = { output with consumed }
let set_value value output = { output with value }
let set_remaining remaining output = { output with remaining }
let remaining { remaining; _ } = remaining
let log { log; _ } = log
let set_log log output = { output with log }

let map f { value; consumed; remaining; log } =
  { value = f value; consumed; remaining; log }

let tag tag { value; consumed; remaining; log } =
  let consumed = Consumed.tag tag consumed in
  { value; consumed; remaining; log }
