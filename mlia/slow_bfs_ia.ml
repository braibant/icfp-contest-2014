(** Avoid polymorphic equality *)
let eq_int m n = (m : int) = n

let rec eq_list eq la lb = match la, lb with
  | [], [] -> true
  | x::xs, y::ys -> eq x y && eq_list eq xs ys
  (* note: or-patterns are not well-supported *)
  | [], _::_ -> false
  | _::_, [] -> false

let eq_pair eqa eqb (a,b) (a',b') =
  eqa a a' && eqb b b'

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
    if eq_int n 0 then x
    else nth (n-1) xs

let rec find p = function
  | [] -> None
  | x::xs ->
    if p x then Some x
    else find p xs

let rec mem eq x = function
  | [] -> false
  | x' :: xs ->
    eq x x' || mem eq x xs

type 'a option =
| None
| Some of 'a

type iter_status =
| Continue
| Stop

let rec fold_left f acc = function
  | [] -> acc
  | x::xs -> fold_left f (f acc x) xs

let rec fold_left_stop f acc = function
  | [] -> Continue, acc
  | x::xs ->
    match f acc x with
      | Stop, v -> Stop, v
      | Continue, acc -> fold_left_stop f acc xs

let rec list_map f = function
  | [] -> []
  | x::xs ->
    let a = f x in a :: list_map f xs

(** Domain-specific library *)
let eq_pos : location -> location -> bool =
  fun (x, y) (x', y') -> eq_int x x' && eq_int y y'

let get map (i, j) =
  nth i (nth j map)

let free map pos =
  get map pos <> Wall

let next_pos direction (x, y) = match direction with
  | Up    -> (x,y-1)
  | Right -> (x+1, y)
  | Down  -> (x, y+1)
  | Left  -> (x-1, y)

(* let directions = [Up; Right; Down; Left] *)
let directions = [Left; Down; Right; Up]

let good_square square =
  square = Pill
  || square = Power_pill
  || square = Fruit

let mem_pos pos li = mem eq_pos pos li

let bfs map ghosts pos =
  let rec loop old cur_gen next_gen =
    match cur_gen with
    | [] ->
      begin match next_gen with
        | [] -> None
        | _ -> loop old next_gen []
      end
    | (pos, start_dir) :: cur_gen ->
      if mem_pos pos old then loop old cur_gen next_gen
      else if good_square (get map pos) then Some start_dir
      else
        let next_gen =
          fold_left
            (fun next_gen dir ->
              let pos = next_pos dir pos in
              if not (free map pos) || mem_pos pos ghosts then next_gen
              else (pos, start_dir) :: next_gen
            ) next_gen directions
        in
        loop (pos :: old) cur_gen next_gen
  in
  let first_gen =
    fold_left
      (fun gen dir ->
        let pos = next_pos dir pos in
        if not (free map pos) then gen
        else (pos, dir) :: gen
      ) [] directions
  in
  loop [pos] first_gen []

let step state world =
  let (map, lambda, ghosts, _fruit) = world in
  let (_vita, pos, lambda_dir, _lives, _score) = lambda in
  let ghost_pos =
    list_map (fun (_vita, pos, dir) -> next_pos dir pos) ghosts in
  let dir =
    match
      bfs map ghost_pos pos
    with
      | None -> lambda_dir
      | Some dir -> dir
  in
  (state, dir)

let main_ia = fun state _world -> (42, step)
