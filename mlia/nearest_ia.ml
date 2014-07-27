
(* The state is
   1. The map;
   2. the status of Lambda-Man;
   3. the status of all the ghosts;
   4. the status of fruit at the fruit location.
*)

(*
  The Lambda-Man status is a 5-tuple consisting of:
  1. Lambda-Man's vitality;
  2. Lambda-Man's current location, as an (x,y) pair;
  3. Lambda-Man's current direction;
  4. Lambda-Man's remaining number of lives;
  5. Lambda-Man's current score.
*)

type 'a option  = None | Some of 'a

type square =
| (* 0 *) Wall
| (* 1 *) Empty
| (* 2 *) Pill
| (* 3 *) Power_pill
| (* 4 *) Fruit
| (* 5 *) Lambda_start
| (* 6 *) Ghost_start

type direction =
| (* 0 *) Up
| (* 1 *) Right
| (* 2 *) Down
| (* 3 *) Left


let rec list_fold_left f acc l =
  match l with
    | [] -> acc
    | t::q -> list_fold_left  f (f acc t) q

let abs x =
  if x < 0 then 0-x else x

let manhattan_distance (a,b) (c,d) =
  abs (a - c) + abs (b - d)

let nearest_pill lman map =
  fst (list_fold_left
    (fun (acc,x) line ->
      list_fold_left
        (fun (acc,y) cell ->
          match cell with
            | 2 ->
              begin match acc with
                | None -> Some (x,y)
                | Some p ->
                  let a = manhattan_distance p lman in
                  let b = manhattan_distance (x,y) lman in
                  if a < b
                  then acc
                  else Some (x,y)
              end, 1 + y
            | _ -> acc, 1 + y
        ) (acc,0) line
    ) (None, 0) map)

let opposite a b =
  (a= Up && b = Down)
  || (b= Up && a = Down)
  || (a= Left && b = Right)
  || (b= Left && a = Right)

let modulo a b =
  a - (a / b) * b

let random n =
  let n' = n * 69069 + 1 in
  if n' < 0 then 0 - n' else n'


let direction_of_int i =
  if i = 0 then Up
  else if i = 1 then Right
  else if i = 2 then Down
  else Left

let step (rnd, old_dir) (map, lman, ghosts, fruits) =
  let rnd =random rnd in
  if modulo (rnd / 54321) 100 = 0
  then
    let new_dir = (modulo (rnd / 54321) 4)  in
    let new_dir = direction_of_int new_dir in
    let direction =
      (* do not U-turn *)
      if opposite old_dir new_dir
      then old_dir
      else new_dir in
    ((rnd, direction), direction)
  else
    begin
      let (vitality, (x,y), direction, lives, score) = lman in
      let (tx,ty) = match nearest_pill (x,y) map with
        |  None -> x+1,y
        |  Some (tx,ty) -> tx,ty
      in
      let dx = abs (x - tx) in
      let dy = abs (y - ty) in
      let d1 = if x <= tx then Right else Left in
      let d2 = if y <= ty then Down  else Up in
      let new_dir =   if dx <= dy then d2 else d1 in
      (* let direction = *)
      (*   if opposite old_dir new_dir *)
      (*   then old_dir *)
      (*   else new_dir *)
      (* in *)
      ((rnd, new_dir), new_dir)
    end
let state = (0,0)
let main_gcc = (state, step)
