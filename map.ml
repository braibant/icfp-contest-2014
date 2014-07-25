type t = Content.t array array

let get map  ~x ~y =
  map.(x).(y)

let set map  ~x ~y =
  map.(x).(y)

let width  map = Array.length map
let height map = Array.length map.(0)

let iter f map =
  for x = 0 to width map - 1 do
    for y = 0 to height map - 1 do
      f ~x ~y map.(x).(y)
    done
  done
