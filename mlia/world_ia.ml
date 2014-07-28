open! World_ffi

open! Lib
open! Lib_list

(** Domain-specific library *)
let get map (i, j) =
  list_nth' i (list_nth' j map)

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
      list_find (fun dir -> free map (next_pos dir pos)) directions
    with
      | None -> assert false
      | Some dir -> dir
  in
  (state, dir)

let main_gcc = (42, step)
