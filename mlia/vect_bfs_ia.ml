open! World_ffi

open! Lib
open! Lib_list
open! Lib_vect

(** Domain-specific library *)
let eq_pos : location -> location -> bool =
  fun (x, y) (x', y') -> eq_int x x' && eq_int y y'

let vect_map map =
  vect_of_list (list_map vect_of_list map)

let map_of_vect vect =
  list_of_vect (map_vect list_of_vect vect)

let nth2 map (i, j) =
  list_nth' i (list_nth' j map)

let get2 vect (i, j) =
  get_vect (get_vect vect j) i

let free map pos =
  get2 map pos <> Wall

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

let mem_pos pos li = list_mem eq_pos pos li

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
      else if good_square (get2 map pos) then Some start_dir
      else
        let next_gen =
          list_fold_left
            (fun next_gen dir ->
              let pos = next_pos dir pos in
              if not (free map pos) || mem_pos pos ghosts then next_gen
              else (pos, start_dir) :: next_gen
            ) next_gen directions
        in
        loop (pos :: old) cur_gen next_gen
  in
  let first_gen =
    list_fold_left
      (fun gen dir ->
        let pos = next_pos dir pos in
        if not (free map pos) then gen
        else (pos, dir) :: gen
      ) [] directions
  in
  loop [pos] first_gen []

let step state world =
  let (map, lambda, ghosts, _fruit) = world in
  let map = vect_map map in
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
