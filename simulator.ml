open Simulator_types
open Simulator_constants

exception Reset_positions
exception Win
exception Lose

module L = struct
  include Simulator_types.L

  let move state lman =
    ()

  let make (x,y) =
    {
      x;
      y;
      tick_to_move = 128;
      lives = 3;
      direction = 2;            (* everybody looks down at the beginning. *)
    }
end

module G = struct

  include Simulator_types.G

  (* 0 is up; 1 is right; 2 is down; 3 is left. *)

  let walls board x y =
    [|
      Board.get board ~x ~y:(y - 1) <> Content.Wall; (* UP *)
      Board.get board ~x:(x+1) ~y   <> Content.Wall; (* RIGHT *)
      Board.get board ~x ~y:(y + 1) <> Content.Wall; (* DOWN *)
      Board.get board ~x:(x-1) ~y   <> Content.Wall; (* LEFT *)
    |]

  let move environment ghost ghc_state =
    let direction = match Ghc.execute environment ghc_state with
      | None -> ghost.direction
      | Some direction -> direction in
    let walls = walls environment.Ghc.map ghost.x ghost.y in
    let direction =
      if walls.(direction)
      then direction
      else if walls.(ghost.direction)
      then ghost.direction
      else if walls.(0)
      then 0
      else if walls.(1)
      then 1
      else if walls.(2)
      then 2
      else 3
    in
    assert walls.(direction);
    match direction with
      | 0 -> ghost.y <- ghost.y - 1
      | 1 -> ghost.x <- ghost.x + 1
      | 2 -> ghost.y <- ghost.y + 1
      | 3 -> ghost.x <- ghost.x - 1
      | _ -> assert false

  let position g =
      g.x, g.y

  let stats g =
    g.vitality, g.direction

  let is_frightened g =
    g.vitality = 1

  let set_next_move utc g =
    if is_frightened g
    then g.tick_to_move <- utc + Delay.ghost_fright.(g.index)
    else g.tick_to_move <- utc + Delay.ghost.(g.index)

  let make (x,y) index =
    {
      x;
      y;
      tick_to_move = Delay.(index mod 4);
      direction = 2;
      vitality = 0;
      index;
    }
end

module Make (M : sig
                   val board : Board.t
                   val ghost_programs : Ghc.code array
                   val lambda_program : Gcc_instr.instruction array
                 end) =
struct
  include M

  let end_of_lives = 127 * Board.width board * Board.height board * 16

  let get: x:int -> y:int -> Content.t = fun ~x ~y ->
    Board.get board x y
  let set: x:int -> y:int -> Content.t -> unit = fun ~x ~y c ->
    Board.set board x y c

  let level = ((Board.width board * Board.height board) / 100) + 1
  let _ = assert (100 * (level - 1) < Board.width board * Board.height board)
  let _ = assert (Board.width board * Board.height board <= 100 * level)

  type state = Simulator_types.state

  let reset_ghost ghost =
    let x,y = (Board.ghosts_start board).(ghost.G.index) in
    ghost.G.x <- x;
    ghost.G.y <- y

  let reset_positions state =
    let x,y = Board.lambda_man_start board in
    state.lambda_man.L.x <- x;
    state.lambda_man.L.y <- y;
    state.lambda_man.L.lives <- state.lambda_man.L.lives - 1;
    Array.iter (fun ghost -> reset_ghost ghost) state.ghosts

  (* Build a GHC environment for a given state *)
  let make_ghc_env game =
    let open Ghc in
    {
      lman_coordinates = [| game.lambda_man.L.x, game.lambda_man.L.y |];
      ghost_starting_positions = Board.ghosts_start board;
      ghost_current_positions = Array.map G.position game.ghosts;
      ghost_stats = Array.map G.stats game.ghosts;
      map = board;
    }

  let gcc_map = make_gcc_map board
  let make_gcc_env game =
    Simulator_ffi.make_gcc_env gcc_map game 

  let eating lman =
    let x = lman.L.x in
    let y = lman.L.y in
    match get ~x ~y with
    | Content.Pill | Content.PowerPill | Content.Fruit -> true
    | _ -> false

  let eat_a_ghost lman ghost =
    reset_ghost ghost

  let tick state =
    let game = state.game in
    let lman = game.lambda_man in
    begin
      (* All Lambda-Man and ghost moves scheduled for this tick take
         place. (Note that Lambda-Man and the ghosts do not move every tick,
         only every few ticks; see the ticks section below.)  *)
      if lman.L.tick_to_move = game.tick
      then
        begin
          L.move state lman;
          if eating lman
          then lman.L.tick_to_move <- game.tick + Delay.eating
          else lman.L.tick_to_move <- game.tick + Delay.not_eating
        end;

      let env = make_ghc_env game in
      Array.iteri
        (fun i ghost ->
         if ghost.G.tick_to_move = game.tick
         then
           begin
             G.set_next_move game.tick ghost;
             G.move env ghost state.ghost_procs.(i);
           end
        ) game.ghosts;
    end;

    begin match game.fright_mode with
          | None -> ()
          | Some time ->
             if time = game.tick
             then game.fright_mode <- None
    end;

    begin let x,y = Board.fruit_position  board in
          if game.tick = Time.fruit_1_appear
             || game.tick = Time.fruit_2_appear
          then set ~x ~y Content.Fruit
          else if (game.tick = Time.fruit_1_expires
                   || game.tick = Time.fruit_2_expires)
                  && get ~x ~y = Content.Fruit
          then set ~x ~y Content.Empty
          else ()
    end;


    begin
      let x = lman.L.x in
      let y = lman.L.y in
      match get ~x ~y with
      | Content.Pill ->
         game.pills <- game.pills - 1;
         set ~x ~y Content.Empty;
      | Content.PowerPill ->
         set ~x ~y Content.Empty;
         game.fright_mode <- Some (game.tick + Time.fright_mode_duration)
      | Content.Fruit ->
         set ~x ~y Content.Empty;
      | _ -> ()
    end;

    (* Next, if one or more visible ghosts are on the same square as
       Lambda-Man, then depending on whether or not fright mode is active,
       Lambda-Man either loses a life or eats the ghost(s). See below for
       details. *)
    begin try
        Array.iter
          (fun ghost ->
           if lman.L.x = ghost.G.x
           && lman.L.y = ghost.G.y
           then
             begin
               (* Check what happens if two lambda-men are eaten at
                  the same point in time. *)
               if ghost.G.vitality = 0
               then raise Reset_positions
               else if ghost.G.vitality = 1
               then eat_a_ghost lman ghost
               else ()
             end
          ) game.ghosts;
      with
        Reset_positions -> reset_positions game
    end;

    (* Next, if all the ordinary pills (ie not power pills) have been
       eaten, then Lambda-Man wins and the game is over. *)
    if game.pills = 0
    then raise Win;

    if game.lambda_man.L.lives = 0
    then raise Lose;

    game.tick <- game.tick + 1

  let ghosts_start = Board.ghosts_start board

  (** Set up the initial values for the various elements of state *)
  let game : game_state =
    let ghosts =
      Array.mapi
        (fun index position -> G.make position index)
        ghosts_start in
    let lambda_man = L.make (Board.lambda_man_start board) in
    let tick = 1 in
    let pills = Board.pills board in
    let fright_mode = None in
    {
      ghosts;
      lambda_man;
      tick;
      pills;
      fright_mode;
    }

  (** Set up the initial values for the coprocessors *)
  let init : state =
    let length_ghost_programs = Array.length ghost_programs in
    let ghost_indices =
      Array.mapi
        (fun index _ -> index mod length_ghost_programs)
        ghosts_start in
    let ghost_codes =
      Array.map (fun index -> ghost_programs.(index)) ghost_indices in
    let ghost_procs = Array.mapi Ghc.init ghost_codes in
    let lambda_proc = Gcc.init_regs in
    let lambda_code = lambda_program in
    {
      game;
      ghost_procs;
      ghost_codes;
      lambda_proc;
      lambda_code;
    }

  (** [repl] stands for Read-Eval-Print-Loop and is the main loop for
  our simulator.  *)
  let repl () =
    let state = init in
    Display.init board;
    try
      while true do
        Display.show board state.game;
        tick state
      done
    with
    | Win -> Format.printf "Lambda-man has won\n"
    | Lose -> Format.printf "Lambda-man has lost\n"
end
