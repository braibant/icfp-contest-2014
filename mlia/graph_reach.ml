open Simu

open! Lib
open! Lib_list
open! Lib_vect
open! Lib_reach

(** {2 map}  *)
let get map (i,j) =
  if i < 0 || j < 0
  then Some Wall
  else
  match list_nth j map with
    | None -> None
    | Some line -> list_nth i line

let get' map (i,j) =
  get_vect (get_vect map j) i

let next_pos direction (x, y) = match direction with
  | Up    -> (x,y-1)
  | Right -> (x+1, y)
  | Down  -> (x, y+1)
  | Left  -> (x-1, y)

(* let directions = [Up; Right; Down; Left] *)
let directions = [Left; Down; Right; Up]

(** Ghost reachability *)

(** {2 Graphs}  *)
type graph = (direction list) list list

let make_graph map =
  let _, graph =
    list_fold_left (fun (j,graph) line ->
      let _, l =
        list_fold_left (fun (i,graph) cell ->
          let pos = i,j in
          match get map pos with
            | None
            | Some Wall ->  1 +i, [] :: graph
            | Some _ ->
              begin
                let edges =
                  list_fold_left
                    (fun edges dir ->
                      match get map (next_pos dir pos) with
                        | None -> edges
                        | Some c ->
                          if Wall = c
                          then edges
                          else dir::edges
                    )
                    [] directions
                in
                1+i, edges::graph
              end
        )
          (0, [])
          line
      in
      1 + j , vect_of_list (list_rev l) :: graph
    )
      (0,[])
      map
  in
  vect_of_list (list_rev graph)

(* let _ = *)
(*   Printf.printf "\n"; *)
(*     for j = 0 to 5 do *)
(*       for i = 0 to 6 do *)
(*         begin match get map (i,j) with *)
(*           | None -> invalid_arg (Printf.sprintf "%i %i" i j) *)
(*           | Some Wall -> Printf.printf "#" *)
(*           | Some _ -> Printf.printf " " *)
(*         end; *)
(*       done; *)
(*     Printf.printf "\n" *)
(*   done;; *)


(* let _ = *)
(*   Printf.printf "\n"; *)
(*     for j = 0 to 5 do *)
(*       for i = 0 to 6 do *)
(*         begin match get_graph tuto (i,j) with *)
(*           | None -> invalid_arg (Printf.sprintf "%i %i" i j) *)
(*           | Some [] -> Printf.printf "#" *)
(*           | Some _ -> Printf.printf " " *)
(*         end; *)
(*       done; *)
(*     Printf.printf "\n" *)
(*   done;; *)

(* let get_graph graph (i,j) = *)
(*   match (list_nth j graph) with *)
(*     | None -> None *)
(*     | Some l ->  list_nth i  l *)

let get_graph' = get2

let get_graph graph (i,j) =
Some (get_graph' graph (i,j))


let mem_pos (pos: location) (li: location list) = list_mem eq_pos pos li

let free map pos =
  get2 map pos <> Wall

let abs x =
  if x < 0 then 0 - x else x

let distance a b =
  abs (fst a - fst b) + abs (snd a - snd b)

let min_int (a: int) b = if a < b then a else b

let ghost_near ghosts pos d =
  list_fold_left (fun acc ghost ->
    if distance ghost pos < d
    then acc + 1
    else acc
  )  0 ghosts

let chase map graph ghosts pos fright_time  =
  let rec loop old cur_gen next_gen distance =
    match cur_gen with
      | [] ->
        begin match next_gen with
          | [] -> None
          | _ -> loop old next_gen [] (distance+1)
        end
      | (pos, path) :: cur_gen ->
        if mem_pos pos old
        then loop old cur_gen next_gen distance
        else if mem_pos pos ghosts && distance < fright_time
        then Some path
        else
          let directions = get_graph' graph pos in
          let next_gen =
            list_fold_left
              (fun next_gen dir ->
                let pos = next_pos dir pos in
                (pos, dir::path) :: next_gen
              ) next_gen directions
          in
          loop (pos :: old) cur_gen next_gen distance
  in
  match get_graph graph pos with
    | None -> None
    | Some directions ->
      let first_gen =
        list_fold_left
          (fun gen dir ->
            let pos = next_pos dir pos in
            if not (free map pos) then gen
            else (pos, [dir]) :: gen
          ) [] directions
      in
      begin
        match loop [pos] first_gen [] 1 with
          | None -> None
          | Some path -> Some (list_rev path)
      end

let small_distance = 3
let small_distances = [0;1;2;3]

let bfs map graph ghosts ghost_places pos =
  let rec loop old cur_gen next_gen distance =
    match cur_gen with
      | [] ->
        begin match next_gen with
          | [] ->
            None
          | _ -> loop old next_gen [] (1 + distance)
        end
      | (pos, path) :: cur_gen ->
        if mem_pos pos old
        then loop old cur_gen next_gen distance
        else
          if good_square (get' map pos)
          then Some path
          else
            let directions = get_graph' graph pos in
            let next_gen =
              list_fold_left
                (fun next_gen dir ->
                  let pos = next_pos dir pos in
                  if mem_pos pos ghosts then next_gen
                  else if distance <= small_distance
                       && mem_pos pos (list_nth' distance ghost_places) then next_gen
                  else (pos, dir::path) :: next_gen
                ) next_gen directions
            in
            loop (pos :: old) cur_gen next_gen distance
  in
  match get_graph graph pos with
    | None -> None
    | Some directions ->
      let first_gen =
        list_fold_left
          (fun gen dir ->
            let pos = next_pos dir pos in
            if not (free map pos) then gen
            else (pos, [dir]) :: gen
          ) [] directions
      in
      begin
        match loop [pos] first_gen [] 0 with
          | None -> None
          | Some path -> Some (list_rev path)
      end

let invalid map graph ghosts pos path =
  fst
    (list_fold_left (fun (invalid,pos) dir ->
      let pos = next_pos dir pos in
      (invalid ||      list_mem eq_pos pos ghosts), pos
     ) (false, pos) path)

let bfs_default map graph ghosts ghosts_pos pos default f =
  let ghosts_pos_dir = list_map (fun (_vita, pos, dir) -> (pos, dir)) ghosts in
  let ghost_places =
    let at_dist (n : int) =
      list_map (fun (pos, _dir) -> pos)
        (reach map graph n ghosts_pos_dir) in
    list_map at_dist small_distances in
  match bfs map graph ghosts_pos ghost_places pos with
    | None -> default
    | Some [] -> default
    | Some (t::q) -> f t q

let step (graph, path) world =
  let (map, lambda, ghosts, _fruit) = world in
  let map = vect_of_map map in
  let (vita, pos, lambda_dir, _lives, _score) = lambda in
  let ghost_pos =
    list_map (fun (_vita, pos, dir) -> next_pos dir pos) ghosts in
  let dir,path =
    if invalid map graph ghost_pos pos path || path = []
    then
      begin
        if vita > 0
          then match chase map graph ghost_pos pos vita with
            | None | Some [] ->
              bfs_default map graph ghosts ghost_pos pos (lambda_dir, [])
                (fun dir path -> dir, path)
            | Some (dir::path) -> dir, path
        else
          bfs_default map graph ghosts ghost_pos pos (lambda_dir, [])
            (fun dir path -> dir, path)
      end
    else match path with
      | dir::path -> dir, path
      | [] -> lambda_dir, []
  in
  ((graph,path), dir)

let state =
  let (map, (lambda, (ghosts, _fruit))) = Simu.world in
  make_graph map,[]

let main_gcc = (state, step)
