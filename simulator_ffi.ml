open Simulator_types

let gcc_int n = Gcc.(tag Int n)

let (@.) a b = Gcc.(tag Pair (a, b))

let tup3 a b c =
  a @. b @. c

let tup4 a b c d =
  a @. b @. c @. d

let tup5 a b c d e =
  a @. b @. c @. d @. e

let rec gcc_list = function
  | [] -> gcc_int 0
  | x::xs -> x @. gcc_list xs

let gcc_array arr =
  Array.to_list arr |> gcc_list

let gcc_list_init n f =
  Array.init n f |> gcc_array

(* Build a GCC representation of a given map *)
let make_gcc_map board =
  let open Content in
  let gcc_content content =
    let int = match content with
      | Wall -> 0
      | Empty -> 1
      | Pill -> 2
      | PowerPill -> 3
      | Fruit -> 4
      | LambdaManStart -> 5
      | GhostStart -> 6
    in gcc_int int
  in
  let open Board in
  gcc_list_init (width board) (fun x ->
    gcc_list_init (height board) (fun y ->
      gcc_content (get board ~x ~y)))

let direction dir = gcc_int dir

let ghost_vitality gv = gcc_int gv

let gcc_loc x y = gcc_int x @. gcc_int y

(* Build a GCC environment for a given game state *)
let make_gcc_env map game =
  let lambda =
    let lman = game.lambda_man in
    let vitality = gcc_int (match game.fright_mode with
      | None -> 0
      | Some future -> future - game.tick)
    in
    let loc = gcc_loc lman.L.x lman.L.y in
    let dir = direction lman.L.direction in
    let lives = gcc_int lman.L.lives in
    let score = gcc_int 0 (* TODO *) in
    tup5
      vitality
      loc
      dir
      lives
      score
  in
  let ghosts =
    let ghost (t : G.t) =
      tup3
        (ghost_vitality t.G.vitality)
        (gcc_loc t.G.x t.G.y)
        (direction t.G.direction)
    in
    Array.map ghost game.ghosts |> gcc_array
  in
  let fruits =
    let status appears expires =
      let tick = game.tick in
      if tick >= appears && tick < expires then tick - expires
      else 0 in
    let open Simulator_constants in
    gcc_int
      (max (status Time.fruit_1_appear Time.fruit_1_expires)
         (status Time.fruit_2_appear Time.fruit_2_expires))
  in
  tup4
    map
    lambda
    ghosts
    fruits
