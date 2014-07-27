let modulo a b =
  a - (a / b) * b

let state = (0, 0)

let opposite a b =
  if (a - b) = 2 then true
  else if (a - b) = -2 then true
  else false

let random n =
  let n' = n * 69069 + 1 in
  if n' < 0 then 0 - n' else n'

let step (rnd, old_dir) _world =
  let rnd = random rnd in
  let new_dir = modulo (rnd / 54321) 4 in
  let direction =
    (* do not U-turn *)
    if opposite old_dir new_dir
    then old_dir
    else new_dir in
  ((rnd, direction), direction)
  
let main_gcc = (state, step)
