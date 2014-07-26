open Simulator_types
open World

let list_init n f =
  (* may change if we decide to produce [flist] instead of [list] *)
  Array.init n f |> Array.to_list

let list_of_array arr =
  Array.to_list arr

(* Build a GCC representation of a given map *)
let make_gcc_map board =
  let open Content in
  let gcc_content : Content.t -> World.square =
    (* this is what happens when different people
       independently need the same data structure *)
    function
      | Wall -> Wall
      | Empty -> Empty
      | Pill -> Pill
      | PowerPill -> Power_pill
      | Fruit -> Fruit
      | LambdaManStart -> Lambda_start
      | GhostStart -> Ghost_start
  in
  let open Board in
  list_init (width board) (fun x ->
    list_init (height board) (fun y ->
      gcc_content (get board ~x ~y)))

let direction = function
  | 0 -> Up
  | 1 -> Right
  | 2 -> Down
  | 3 -> Left
  | _ -> failwith "invalid direction"

let ghost_vitality = function
  | 0 -> Standard
  | 1 -> Fright_mode
  | 2 -> Invisible
  | _ -> failwith "invalid vitality"

(* Build a GCC environment for a given game state *)
let make_gcc_env map game =
  let lambda =
    let lman = game.lambda_man in
    let vitality = match game.fright_mode with
      | None -> 0
      | Some future -> future - game.tick
    in
    let loc = (lman.L.x, lman.L.y) in
    let dir = direction lman.L.direction in
    let lives = lman.L.lives in
    let score = 0 (* TODO *) in
    { vitality; loc; dir; lives; score; }
  in
  let ghosts =
    let ghost (t : G.t) = {
      vitality = ghost_vitality t.G.vitality;
      loc = (t.G.x, t.G.y);
      dir = direction t.G.direction;
    } in
    Array.map ghost game.ghosts |> list_of_array
  in
  let fruits =
    let status appears expires =
      let tick = game.tick in
      if tick >= appears && tick < expires then tick - expires
      else 0 in
    let open Simulator_constants in
    max (status Time.fruit_1_appear Time.fruit_1_expires)
        (status Time.fruit_2_appear Time.fruit_2_expires)
  in
  { map; lambda; ghosts; fruits; }
