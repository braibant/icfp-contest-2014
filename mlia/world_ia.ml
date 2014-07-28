open World_ffi

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

(** Domain-specific library *)
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

let step state world =
  let (map, lambda, _ghost, _fruit) = world in
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
