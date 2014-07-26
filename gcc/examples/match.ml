type list =
  | Nil
  | Nil1
  | Cons of int * list
  | Cons2 of int * list

let main_gcc =
  let rec sum (l:list) : int =
    match l with
    | Nil -> 0
    | Nil1 -> 1
    | Cons (a,l) -> a + (sum l)
    | Cons2 (a,l) -> 1+ a + (sum l)

  in
  sum (Cons(3,Cons2(1,Nil)))
