open Simu

open! Lib
open! Liblist
open! Lib_pq


(** {2 priority queue}*)

(* type priority = int *)
(* type 'a priority_queue = *)
(*     Empty *)
(*   | Node of (priority * ('a * ('a priority_queue * 'a priority_queue))) *)

(* let empty = Empty *)

(* let rec insert queue prio elt = *)
(*   match queue with *)
(*       Empty -> Node(prio, (elt, (Empty, Empty))) *)
(*     | Node(p, e, left, right) -> *)
(*       if prio <= p *)
(*       then Node(prio, elt, insert right p e, left) *)
(*       else Node(p, e, insert right prio elt, left) *)

(* let (>>=) e f = *)
(*   match e with *)
(*     | None -> None *)
(*     | Some e -> f e *)

(* let rec remove_top = function *)
(*   | Empty -> None *)
(*   | Node(prio, elt, left, Empty) -> Some left *)
(*   | Node(prio, elt, Empty, right) -> Some right *)
(*   | Node(prio, elt, (Node(lprio, lelt, _, _) as left), *)
(*          (Node(rprio, relt, _, _) as right)) -> *)
(*     if lprio <= rprio *)
(*     then *)
(*       remove_top left >>= fun left -> *)
(*       Some (Node(lprio, lelt, left, right)) *)
(*     else *)
(*       remove_top right >>= fun right -> *)
(*       Some (Node(rprio, relt, left, right)) *)

(* let extract_top = function *)
(*   | Empty -> None *)
(*   | Node(prio, elt, _, _) as queue -> *)
(*     remove_top queue >>= fun q -> *)
(*       Some (prio, elt, q) *)
>>>>>>> Graph

(** {2 map}  *)
let get map (i,j) =
  if i < 0 || j < 0
  then Some Wall
  else
  match list_nth j map with
    | None -> None
    | Some line -> list_nth i line

let get' map (i,j) =
  list_nth' i (list_nth' j map)

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
  match (list_nth j graph) with
    | None -> None
    | Some l ->  list_nth i  l

let get_graph' graph (i,j) =
  list_nth' i (list_nth' j graph)

let good_square square =
  square = Pill
  || square = Power_pill
  || square = Fruit

let eq_pos : location -> location -> bool =
  fun (x, y) (x', y') -> eq_int x x' && eq_int y y'

let mem_pos (pos: location) (li: location list) = list_mem eq_pos pos li

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
              list_fold_left
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
        list_fold_left
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

let step (graph) world =
  let (map, lambda, ghosts, _fruit) = world in
  let (vita, pos, lambda_dir, _lives, _score) = lambda in
  let ghost_pos =
    list_map (fun (_vita, pos, dir) -> next_pos dir pos) ghosts in
  let dir =
    begin
      if vita > 0
      then
        match chase map graph ghost_pos pos vita with
          | None ->
            begin match
                bfs map graph ghost_pos pos
              with
                | None -> lambda_dir
                | Some (dir::path) -> dir
            end
          | Some (dir :: path) -> dir
      else
        begin match
            bfs map graph ghost_pos pos
          with
            | None -> lambda_dir
            | Some (dir::path) -> dir
        end
    end
  in
  (graph, dir)

let state =
  let (map, (lambda, (ghosts, _fruit))) = Simu.world in
  make_graph map

let main_gcc = (state, step)
