let step state _world =
  let next_state = state + 1 in
  let next_state = if next_state = 3 then 0 else next_state in
  let direction = next_state in
  (next_state, direction)
  
let main_gcc = (0, step)
