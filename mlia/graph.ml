open Simu

open! Lib
open! Liblist
open! Lib_pq

(** {2 map}  *)
let get map (i,j) =
  if i < 0 || j < 0
  then Some Wall
  else
  match nth j map with
    | None -> None
    | Some line -> nth i line

let get' map (i,j) =
  nth' i (nth' j map)

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
    fold_left (fun (j,graph) line ->
      let _, l =
        fold_left (fun (i,graph) cell ->
          let pos = i,j in
          match get map pos with
            | None
            | Some Wall ->  1 +i, [] :: graph
            | Some _ ->
              begin
                let edges =
                  fold_left
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
      1 + j , list_rev l :: graph
    )
      (0,[])
      map
  in
  list_rev graph

let map =
  [
    [Wall ; Wall ; Wall ; Wall  ; Wall  ; Wall ; Wall ];
    [Wall ; Wall ; Empty; Wall  ; Wall  ; Wall ; Wall ];
    [Wall ; Empty; Empty; Empty ; Empty ; Empty; Wall ];
    [Wall ; Wall ; Empty ; Wall ; Wall  ; Pill ; Wall ];
    [Wall ; Wall ; Empty ; Wall ; Wall  ; Wall ; Wall ];
    [Wall ; Wall ; Wall ; Wall  ; Wall  ; Wall ; Wall ];
  ]

let graph = make_graph map

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

let get_graph graph (i,j) =
  match (nth j graph) with
    | None -> None
    | Some l ->  nth i  l

let get_graph' graph (i,j) =
  nth' i (nth' j graph)

let dijkstra graph (lam_x,lam_y) =
  let queue = pq_empty in
  let ans = list_map (fun col -> list_map (fun _ -> None)) graph in
  let () = () in ()

let good_square square =
  square = Pill
  || square = Power_pill
  || square = Fruit

let eq_pos : location -> location -> bool =
  fun (x, y) (x', y') -> eq_int x x' && eq_int y y'

let mem_pos (pos: location) (li: location list) = mem eq_pos pos li

let free map pos =
  match get map pos with
    | None -> false
    | Some c -> c <> Wall

(* let is_some = function *)
(*   | Some a -> a *)
(*   | _ -> assert false *)

let bfs map graph ghosts pos =
  let rec loop old cur_gen next_gen =
    match cur_gen with
      | [] ->
        begin match next_gen with
          | [] ->
            None
          | _ -> loop old next_gen []
        end
      | (pos, path) :: cur_gen ->
        if mem_pos pos old then loop old cur_gen next_gen
        else
          if good_square (get' map pos)
          then Some path
          else
            let directions = get_graph' graph pos in
            let next_gen =
              fold_left
                (fun next_gen dir ->
                  let pos = next_pos dir pos in
                  if mem_pos pos ghosts then next_gen
                  else (pos, dir::path) :: next_gen
                ) next_gen directions
            in
            loop (pos :: old) cur_gen next_gen
  in
  match get_graph graph pos with
    | None -> None
    | Some directions ->
      let first_gen =
        fold_left
          (fun gen dir ->
            let pos = next_pos dir pos in
            if not (free map pos) then gen
            else (pos, [dir]) :: gen
          ) [] directions
      in
      begin
        match loop [pos] first_gen [] with
          | None -> None
          | Some path -> Some (list_rev path)
      end

let step graph world =
  let (map, lambda, ghosts, _fruit) = world in
  let (_vita, pos, lambda_dir, _lives, _score) = lambda in
  let ghost_pos =
    list_map (fun (_vita, pos, dir) -> next_pos dir pos) ghosts in
  let dir =
    match
      bfs map graph ghost_pos pos
    with
      | None -> lambda_dir
      | Some (dir::_) -> dir
  in
  (graph, dir)

let state =
  let (map, (lambda, (ghosts, _fruit))) = Simu.world in
  make_graph map

let main_gcc = (state, step)
