open Simulator_types

exception Reset_positions
exception Win
exception Lose


module Delay = struct
  let eating = 127
  let not_eating = 137

  let ghost = [| 	130; 132; 134; 136 |]
  let ghost_fright = [| 195; 198; 201; 204 |]

  let fright_mode_duration = 127 * 20
end

module L = struct
  include Simulator_types.L

  let move state lman =
    ()

end

module G = struct

  include Simulator_types.G

  let move environment ghost =
    match Ghc.execute environment ghost.ghc with
    | None                    (* no new direction *)
    | Some _ ->                 (* new direction *)
       assert false

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

end

module Make (M : MAP) =
struct

  let get: x:int -> y:int -> Content.t = fun ~x ~y ->
    M.data.(y).(x)
  let set: x:int -> y:int -> Content.t -> unit = fun ~x ~y c ->
    M.data.(y).(x) <- c

  let level = ((M.width * M.height) / 100) + 1
  let _ = assert (100 * (level - 1) < M.width * M.height)
  let _ = assert (M.width * M.height <= 100 * level)

  type state = Simulator_types.state

  module Time = struct
    let end_of_lives = 127 * M.width * M.height * 16

    let fruit_1_appear = 127 * 200
    let fruit_2_appear = 127 * 400
    let fruit_1_expires = 127 * 280
    let fruit_2_expires = 127 * 480
    let fright_mode_duration = 127 * 20
  end

  let reset_ghost ghost =
    let x,y = M.ghosts_start.(ghost.G.index) in
    ghost.G.x <- x;
    ghost.G.y <- y

  let reset_positions state =
    let x,y = M.lambda_man_start in
    state.lambda_man.L.x <- x;
    state.lambda_man.L.y <- y;
    state.lambda_man.L.lives <- state.lambda_man.L.lives - 1;
    Array.iter (fun ghost -> reset_ghost ghost) state.ghosts

  (* Build a GHC environment for a given state *)
  let make_ghc_env state =
    let open Ghc in
    {
      lman_coordinates = [| state.lambda_man.L.x, state.lambda_man.L.y |];
      ghost_starting_positions = M.ghosts_start;
      ghost_current_positions = Array.map G.position state.ghosts;
      ghost_stats = Array.map G.stats state.ghosts;
      map = M.data;
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
          then
            lman.L.tick_to_move <- state.tick + Delay.eating
          else
            lman.L.tick_to_move <- state.tick + Delay.not_eating
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

    begin
      (* Next, any actions (fright mode deactivating, fruit *)
      (* appearing/disappearing) take place. *)
      ()
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
         state.fright_mode <- Some (state.tick + Delay.fright_mode_duration)
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

end
