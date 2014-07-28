(* open Simu *)

open! World_ffi (* to remove as it duplicates `open Simu` *)
open! Lib
open! Lib_list
open! Lib_vect

let vect_map map =
  vect_of_list (list_map vect_of_list map)

let rec list_rev_append_map f li =
  match li with
    | [] -> []
    | x::xs ->
      list_rev_append (f x) (list_rev_append_map f xs)

let next_pos direction (x, y) = match direction with
  | Up    -> (x,y-1)
  | Right -> (x+1, y)
  | Down  -> (x, y+1)
  | Left  -> (x-1, y)

let get2 vect (i, j) =
  get_vect (get_vect vect j) i

let free map pos =
  get2 map pos <> Wall

let directions = [Up; Right; Left; Down]

let eq_pos : location -> location -> bool =
  fun (x, y) (x', y') -> eq_int x x' && eq_int y y'
let mem_pos pos li = list_mem eq_pos pos li

let eq_dir (a : direction) b = (a = b)

let mem_dir direction li = list_mem eq_dir direction li
 
let ghost_step map graph (pos, dir) =
  let frees = get2 graph pos in
  let dirs =
    if mem_dir dir frees then [dir]
    else match frees with
      | [_] ->
          (* only one solution: the only one *)
        frees
      | _ ->
        (* several solution: turning back is forbidden
           not implemented yet *)
        frees
  in
  list_map (fun dir -> (next_pos dir pos, dir)) dirs

(* [reach map graph n ghost_locations]
   is the set of positions one of the ghosts placed in
   [ghost_locations] may be in after [n] steps *)
let rec reach
    (map : square vect vect) 
    (graph : direction list vect vect)
    (n : int) 
    (ghosts : (location * direction) list)
    : (location * direction) list =
  if n = 0 then ghosts
  else
    let next_ghosts = list_rev_append_map 
      (fun pos -> ghost_step map graph pos) ghosts in
    reach map graph (n - 1) next_ghosts
