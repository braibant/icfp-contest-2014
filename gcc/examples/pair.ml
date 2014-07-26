let f t = match t with (x,y) -> x + y

let main_gcc =
  let x = (4,5) in (f x,(x,x))
