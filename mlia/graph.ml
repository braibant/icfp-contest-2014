open Simu

open! Lib
open! Lib_list
open! Lib_vect
open! Lib_pq

let vect_map map =
  vect_of_list (list_map vect_of_list map)

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

let get_graph' graph (i,j) =
  get_vect (get_vect graph j) i

let get_graph graph (i,j) =
Some (get_graph' graph (i,j))

let good_square square =
  square = Pill
  || square = Power_pill
  || square = Fruit

let eq_pos : location -> location -> bool =
  fun (x, y) (x', y') -> eq_int x x' && eq_int y y'

let mem_pos (pos: location) (li: location list) = list_mem eq_pos pos li

let free map pos =
  get' map pos <> Wall

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

let bfs map graph ghosts pos =
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

let bfs_default map graph ghosts pos default f =
  match bfs map graph ghosts pos with
    | None -> default
    | Some [] -> default
    | Some (t::q) -> f t q

let step (graph, path) world =
  let (map, lambda, ghosts, _fruit) = world in
  let map = vect_map map in
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
              bfs_default map graph ghost_pos pos (lambda_dir, [])
                (fun dir path -> dir, path)
            | Some (dir::path) -> dir, path
        else
          bfs_default map graph ghost_pos pos (lambda_dir, [])
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
