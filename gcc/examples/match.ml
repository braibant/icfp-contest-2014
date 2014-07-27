(*
type list =
  | Nil
  | Nil1
  | Cons of int * list

let x =
  let rec sum (l:list) : int =
    match l with
    | Nil -> 0
    | Nil1 -> 1
    | Cons (a,l) -> a + (sum l)

  in
  sum (Cons(3,Cons(1,Nil)))
*)
open Simu

let good_square = function
  | Pill | Fruit -> 1
  | _ -> 2

let main_gcc = [good_square Wall;
                good_square Empty;
                good_square Pill;
                good_square Power_pill;
                good_square Fruit;
                good_square Lambda_start;
                good_square Ghost_start]

(*

type case =
  | A | B | C | D

let f x y = match x,y with
  | A, B -> 1
  | C,D -> 2
  | D,A -> 3
  | _ -> 4

let main_gcc = [f A B]

             (*;f C A; f D A; f B D; f B C]*)

*)
