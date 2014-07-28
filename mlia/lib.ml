open Simu

let eq_int m n = (m : int) = n

let eq_pos : location -> location -> bool =
  fun (x, y) (x', y') -> eq_int x x' && eq_int y y'

let eq_dir a b = match a, b with
  | Up, Up -> true
  | Down, Down -> true
  | Left, Left -> true
  | Right, Right -> true
  | _, _ -> false

let good_square square =
  square = Pill
  || square = Power_pill
  || square = Fruit

let next_pos direction (x, y) = match direction with
  | Up    -> (x,y-1)
  | Right -> (x+1, y)
  | Down  -> (x, y+1)
  | Left  -> (x-1, y)
