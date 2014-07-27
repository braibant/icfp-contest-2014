let modulo a b =
  a - (a / b) * b

let random n = modulo (n * 1103515245 + 12345) 2147483648
let step state _world =
  let next_state = random state in
  let direction = modulo (state / 54321) 4 in
  (next_state, direction)
  
let main_gcc = (0, step)
