let rec f (i:int) (j:int) : int = if i > 10 then i else f (i+1) 1

let main_gcc =
  f 0 1
