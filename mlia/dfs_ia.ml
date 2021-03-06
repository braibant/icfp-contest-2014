
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

type graph  =
    (location * (dir * location) list) list

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

let min_opt (a: int option) b =
  match a with
    | None -> b
    | Some x ->
      begin match b with
        | None -> Some x
        | Some y -> if x < y then Some x else Some y
      end

(* precompute the graph *)

let dfs map ghosts pos =
  let rec loop pos length =
    if 6 < length
    then None
    else
      begin
        let content = get map pos in
        if good_square content
        then Some length
        else if content = Wall
        then None
        else
          fold_left
            (fun res dir ->
              min_opt res (loop (next_pos dir pos)  (1+length))
            ) None directions
      end
  in
  fold_left
    (fun res dir ->
      match res with
        | None ->
          begin match loop (next_pos dir pos) 0 with
            | None -> None
            | Some length -> Some (dir, length)
          end
        | Some (d, l) ->
          begin match (loop (next_pos dir pos) 0) with
            | None -> res
            | Some length ->
              if (l:int) < length then res else Some (dir, length)
          end
    )
    None
    directions

(* let pill map pos = *)
(*   fold_left *)
(*     (fun res dir -> *)
(*       match res with *)
(*         | None *)
(*     ) *)
(*     None directions *)

let direction_of_int i =
  if eq_int i 0 then Up
  else if eq_int i 1 then Right
  else if eq_int i 2 then Down
  else Left

let modulo a b =
  a - (a / b) * b

let step state world =
  let (map, lambda, ghosts, _fruit) = world in
  let (_vita, pos, lambda_dir, _lives, _score) = lambda in
  let ghost_pos =
    list_map (fun (_vita, pos, dir) -> next_pos dir pos) ghosts in
  let dir =
    (* let _ = (dfs map ghost_pos pos) in *)
    (* direction_of_int (modulo state 4)a *)
    match dfs map ghost_pos pos with
      | None -> direction_of_int (modulo state 4)
      | Some (dir,_) -> dir
  in
  (1 + state, dir)

let state = 42
let main_gcc = (state,step)
