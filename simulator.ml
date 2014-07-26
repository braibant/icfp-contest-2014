open Simulator_types



exception Reset_positions
exception Win
exception Lose


module Delay = struct
  let eating = 127
  let not_eating = 137

  let ghost = [| 	130; 132; 134; 136 |]
  let ghost_fright = [| 195; 198; 201; 204 |]
end

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

  let move environment ghost =
    let direction = match Ghc.execute environment ghost.ghc with
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

  let make (x,y) index program_index program =
    {
      ghc = Ghc.init index program;
      x;
      y;
      tick_to_move = Delay.(index mod 4);
      direction = 2;
      vitality = 0;
      index;
      program_index
    }
end

module Make (M : sig
                   val board : Board.t
                   val ghost_programs : Ghc.code array
                 end) =
struct
  include M

  let get: x:int -> y:int -> Content.t = fun ~x ~y ->
    Board.get board x y
  let set: x:int -> y:int -> Content.t -> unit = fun ~x ~y c ->
    Board.set board x y c

  let level = ((Board.width board * Board.height board) / 100) + 1
  let _ = assert (100 * (level - 1) < Board.width board * Board.height board)
  let _ = assert (Board.width board * Board.height board <= 100 * level)

  type state = Simulator_types.state

  module Time = struct
    let end_of_lives = 127 * Board.width board * Board.height board * 16

    let fruit_1_appear = 127 * 200
    let fruit_2_appear = 127 * 400
    let fruit_1_expires = 127 * 280
    let fruit_2_expires = 127 * 480
    let fright_mode_duration = 127 * 20
  end

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
  let make_ghc_env state =
    let open Ghc in
    {
      lman_coordinates = [| state.lambda_man.L.x, state.lambda_man.L.y |];
      ghost_starting_positions = Board.ghosts_start board;
      ghost_current_positions = Array.map G.position state.ghosts;
      ghost_stats = Array.map G.stats state.ghosts;
      map = board;
    }

  let eating lman =
    let x = lman.L.x in
    let y = lman.L.y in
    match get ~x ~y with
    | Content.Pill | Content.PowerPill | Content.Fruit -> true
    | _ -> false

  let eat_a_ghost lman ghost =
    reset_ghost ghost

  let tick state =
    let lman = state.lambda_man in
    begin
      (* All Lambda-Man and ghost moves scheduled for this tick take
         place. (Note that Lambda-Man and the ghosts do not move every tick,
         only every few ticks; see the ticks section below.)  *)
      if lman.L.tick_to_move = state.tick
      then
        begin
          L.move state lman;
          if eating lman
          then lman.L.tick_to_move <- state.tick + Delay.eating
          else lman.L.tick_to_move <- state.tick + Delay.not_eating
        end;

      let env = make_ghc_env state in
      Array.iter
        (fun ghost ->
         if ghost.G.tick_to_move = state.tick
         then
           begin
             G.set_next_move state.tick ghost;
             G.move env ghost;
           end
        ) state.ghosts;
    end;

    begin match state.fright_mode with
          | None -> ()
          | Some time ->
             if time = state.tick
             then state.fright_mode <- None
    end;

    begin let x,y = Board.fruit_position  board in
          if state.tick = Time.fruit_1_appear
             || state.tick = Time.fruit_2_appear
          then set ~x ~y Content.Fruit
          else if (state.tick = Time.fruit_1_expires
                   || state.tick = Time.fruit_2_expires)
                  && get ~x ~y = Content.Fruit
          then set ~x ~y Content.Empty
          else ()
    end;


    begin
      let x = lman.L.x in
      let y = lman.L.y in
      match get ~x ~y with
      | Content.Pill ->
         state.pills <- state.pills - 1;
         set ~x ~y Content.Empty;
      | Content.PowerPill ->
         set ~x ~y Content.Empty;
         state.fright_mode <- Some (state.tick + Time.fright_mode_duration)
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
          ) state.ghosts;
      with
        Reset_positions -> reset_positions state
    end;

    (* Next, if all the ordinary pills (ie not power pills) have been
       eaten, then Lambda-Man wins and the game is over. *)
    if state.pills = 0
    then raise Win;

    if state.lambda_man.L.lives = 0
    then raise Lose;

    state.tick <- state.tick + 1

  (** Set up the initial values for the various elements of state *)
  let init : state =
    let length_ghost_programs = Array.length ghost_programs in
    let ghosts = Array.mapi
                   (fun index position ->
                    let program_index =
                      index mod length_ghost_programs
                    in
                    G.make
                      position
                      index
                      program_index
                      (ghost_programs.(program_index))
                   )
                   (Board.ghosts_start board) in
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

  (** [repl] stands for Read-Eval-Print-Loop and is the main loop for
  our simulator.  *)
  let repl () =
    let state = init in
    Display.init board;
    try
      while true do
        Display.show board state;
        tick state
      done
    with
    | Win -> Format.printf "Lambda-man has won\n"
    | Lose -> Format.printf "Lambda-man has lost\n"
end
