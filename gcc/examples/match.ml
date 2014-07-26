type list =
  | Nil
  | Nil1
  | Cons of int * list

let main_gcc =
  let rec sum (l:list) : int =
    match l with
    | Nil -> 0
    | Nil1 -> 1
    | Cons (a,l) -> a + (sum l)

  in
  sum (Cons(3,Cons(1,Nil)))
