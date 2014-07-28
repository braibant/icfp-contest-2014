 open Simu

open! Lib
open! Lib_list
open! Lib_vect

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
      1 + j , vect_of_list (list_rev l) :: graph
    )
      (0,[])
      map
  in
  vect_of_list (list_rev graph)

let map =
  [
    [Wall ; Wall ; Wall ; Wall  ; Wall  ; Wall ; Wall ];
    [Wall ; Wall ; Empty; Wall  ; Wall  ; Wall ; Wall ];
    [Wall ; Empty; Empty; Empty ; Pill ; Pill ; Wall ];
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

let get_graph' graph (i,j) =
  get_vect  (get_vect graph j) i

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
  match get map pos with
    | None -> false
    | Some c -> c <> Wall

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


(*
   explore all the possible paths of length [length]
*)

let dfs map graph ghosts pos length fright=
  let select (best: 'a option) (candidate: 'a option) : 'a option =
    match best with
      | None -> candidate
      | Some (pa,da,va) ->
        begin match candidate with
          | None -> best
          | Some (pb,db,vb) ->
            if va < vb then candidate else best
        end
  in
  let rec loop pos distance path dirs value fright =
    if distance = length
    then Some (path, dirs, value)
    else if mem_pos pos path
    then
      begin
        None
      end
    else
      begin
        let content = get map pos in
        let value =
          if fright > 1 && mem_pos pos ghosts
          then value + 200
          else value in
        let value, fright  =
          match content with
            | Some Pill -> value + 10, fright
            | Some Power_pill -> value + 50, fright + 20
            | _ -> value, fright
        in
        let value =
          if fright <= 0 && mem_pos pos ghosts
          then value - 1000
          else value
        in
        let directions = get_graph' graph pos in
        list_fold_left (fun best dir ->
          let npos = next_pos dir pos in
          let candidate = loop npos (distance + 1) (pos::path) (dir::dirs) value (fright - 1) in
          select best candidate
        ) (Some (path, dirs, value)) directions
      end
  in
  match loop pos 0 [] [] 0 fright with
    | None -> None
    | Some (path, dirs, value) -> Some (list_rev path, list_rev dirs, value)
;;


let find_pill map graph pos =
  let rec loop pos path dirs length =
    if mem_pos pos path || length > 20
    then None
    else
      begin
        let content = get map pos in
        match content with
          | Some Pill
          | Some Power_pill ->
            Some (list_rev dirs)
          | _ ->
          let directions = get_graph' graph pos in
          list_fold_left (fun acc dir ->
            match acc with
              | None ->
                let npos = (next_pos dir pos) in
                loop npos (pos::path) (dir::dirs) (1+ length)
              | Some _ ->  acc
          ) None directions
      end
  in
  loop pos [] [] 0

(* let _ = dfs map graph [] (2,2) 5 5;; *)

let invalid map graph ghosts pos path =
  fst
    (list_fold_left (fun (invalid,pos) dir ->
      let pos = next_pos dir pos in
      (invalid ||      list_mem eq_pos pos ghosts), pos
     ) (false, pos) path)

let step (graph, path, lives) world =
  let (map, lambda, ghosts, _fruit) = world in
  let (vita, pos, lambda_dir, nlives, _score) = lambda in
  let path = if (lives: int) > nlives then  [] else path in
  let ghost_pos =
    list_map (fun (_vita, pos, dir) -> next_pos dir pos) ghosts in
  let dir,path =
    if invalid map graph ghost_pos pos path || path = []
    then
      begin
        match dfs map graph ghost_pos pos 10 vita with
          | None | Some (_,[],_) ->
            begin match find_pill map graph pos with
              | None | Some [] -> lambda_dir, []
              | Some (dir::path) -> dir, path
            end
          | Some (_,dir::path,_) ->  dir, path
      end
    else match path with
      | dir::path -> dir, path
      | [] -> lambda_dir, []
  in
  ((graph,path,nlives), dir)

let state =
  let (map, (lambda, (ghosts, _fruit))) = Simu.world in
  make_graph map,[], 3

let main_gcc = (state, step)
