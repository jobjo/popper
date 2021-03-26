type t =
  { tag : Tag.t
  ; data : Int32.t list
  }

let make tag data = { tag; data }
let tag { tag; _ } = tag
let data { data; _ } = data
