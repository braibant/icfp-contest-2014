let rec fac n =
  if n = 0 then 1
  else n * fac (n - 1)

let rec fac_tail n acc =
  if n = 0 then acc
  else fac_tail (n - 1) (n * acc)

let main_gcc = (fac 8, fac_tail 6 1)
