type 'a t =
  { value : 'a
  ; consumed : Consumed.t
  ; remaining : Input.t
  ; log : Log.t
  ; max_size : int
  }

let make ~value ~max_size ~consumed ~remaining ~log =
  { max_size; value; consumed; remaining; log }

let value { value; _ } = value
let max_size { max_size; _ } = max_size
let set_max_size max_size v = { v with max_size }
let consumed { consumed; _ } = consumed
let set_consumed consumed output = { output with consumed }
let set_value value output = { output with value }
let set_remaining remaining output = { output with remaining }
let remaining { remaining; _ } = remaining
let log { log; _ } = log
let set_log log output = { output with log }

let map f { max_size; value; consumed; remaining; log } =
  { value = f value; max_size; consumed; remaining; log }

let tag tag { value; max_size; consumed; remaining; log } =
  let consumed = Consumed.tag tag consumed in
  { value; max_size; consumed; remaining; log }
