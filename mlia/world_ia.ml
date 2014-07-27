(** World FFI *)
type ghost_vitality =
| (* 0 *) Standard
| (* 1 *) Fright_mode
| (* 2 *) Invisible

type square =
| (* 0 *) Wall
| (* 1 *) Empty
| (* 2 *) Pill
| (* 3 *) Power_pill
| (* 4 *) Fruit
| (* 5 *) Lambda_start
| (* 6 *) Ghost_start

type direction =
| (* 0 *) Up
| (* 1 *) Right
| (* 2 *) Down
| (* 3 *) Left

type location = int * int

type fruit_status = int

type lambda_status =
  ( lambda_vitality
  * location
  * direction
  * lives
  * score )
and lambda_vitality = int
and lives = int
and score = int

(*
type lambda_status = {
  vitality : int;
  loc : location;
  dir : direction;
  lives : int;
  score : int;
}
*)

(*
type ghost_status = {
  vitality : ghost_vitality;
  loc : location;
  dir : direction;
}
*)

type ghost_status =
  ( ghost_vitality
  * location
  * direction )
(*
type t = {
  map : map;
  lambda : lambda_status;
  ghosts : ghost_status list;
  fruits : fruit_status;
}
*)
type world =
  ( map
  * lambda_status
  * ghost_status list
  * fruit_status )
and map = square list list


(** Standard Library *)
let rec nth n = function
  | [] -> assert false
  | x::xs ->
    if n = 0 then x
    else nth (n-1) xs

let rec find p = function
  | [] -> assert false
  | x::xs ->
    if p x then x
    else find p xs

let get map (i, j) =
  nth i (nth j map)

let free map pos =
  get map pos <> Wall

let next_pos direction (x, y) = match direction with
  | Up    -> (x,y-1)
  | Right -> (x+1, y)
  | Down  -> (x, y+1)
  | Left  -> (x-1, y)

let directions = [Up; Right; Down; Left]

let step state world =
  let (map, lambda, _ghost, _fruit) = world in
  let (_vita, pos, _dir, _lives, _score) = lambda in
  let dir =
    find (fun dir -> free map (next_pos dir pos)) directions
  in
  (state, dir)

let main_ia = fun state _world -> (42, step)
