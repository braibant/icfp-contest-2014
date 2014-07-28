open Simu

open! Lib
open! Lib_list
open! Lib_vect

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
