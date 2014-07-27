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
  | [] -> None
  | x::xs ->
    if p x then Some x
    else find p xs

let rec mem eq x = function
  | [] -> false
  | x :: xs ->
    eq x || mem eq x xs

type 'a option =
| None
| Some of 'a

type iter_status =
| Continue
| Stop

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

let list_length li =
  let rec loop acc = function
  | [] -> acc
  | _::li -> loop (acc+1) li
  in loop 0 li

(** log-trees *)

type 'a vect_tree = Tree : ('a, 'b) vect_tag * 'b -> 'a vect_tree
and ('a, _) vect_tag =
| Leaf : ('a, 'a) vect_tag
| Node : ('a, 'a vect_tree * 'a vect_tree) vect_tag
type 'a vect = 'a vect_tree * int

let get_vect (t, n) i =
  let rec find : type a . int -> int -> a vect_tree -> a =
    fun i n -> function
    | Tree (Leaf, v) -> v
    | Tree (Node, (left, right)) ->
      let mid = n / 2 in
      if i < mid
      then find i mid left
      else find (i - mid) (n - mid) right
  in find i n t

let rec vect_of_list li =
  let rec consume li n =
    if n = 1 then
      let (x::xs) = li in (Tree (Leaf, x), xs)
    else begin
      let mid = n / 2 in
      let left, li = consume li mid in
      let right, li = consume li (n - mid) in
      Tree (Node, (left, right)), li
    end
  in
  let len = list_length li in
  match consume li len with
    | tree, [] -> (tree, len)
    | _, _::_ -> assert false


(** Domain-specific library *)
let vect_of_map map =
  (map, vect_of_list (list_map vect_of_list map))

let get (map, vect) (i, j) =
  let s1 = nth i (nth j map) in
  let _s2 = get_vect (get_vect vect j) i in
  s1

let free map pos =
  get map pos <> Wall

let next_pos direction (x, y) = match direction with
  | Up    -> (x,y-1)
  | Right -> (x+1, y)
  | Down  -> (x, y+1)
  | Left  -> (x-1, y)

(* let directions = [Up; Right; Down; Left] *)
let directions = [Left; Down; Right; Up]

let step state world =
  let (map, lambda, _ghost, _fruit) = world in
  let map = vect_of_map map in
  let (_vita, pos, _dir, _lives, _score) = lambda in
  let dir =
    match
      find (fun dir -> free map (next_pos dir pos)) directions
    with
      | None -> assert false
      | Some dir -> dir
  in
  (state, dir)

let main_gcc = (42, step)
