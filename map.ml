type t = Content.t array array

let get map  ~x ~y =
  map.(y).(x)

let set map  ~x ~y =
  map.(y).(x)
