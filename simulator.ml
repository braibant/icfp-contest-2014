module type MAP = sig
    val width : int
    val height: int
    val data: Content.t array array (* a mutable array of lines *)
    val lambda_man_start: int * int
    val ghosts_start: (int * int) array
  end

exception Reset_positions
exception Win
exception Lose

module Make (Map : MAP) =
struct

  let get: x:int -> y:int -> Content.t = fun ~x ~y ->
    Map.data.(y).(x)
  let set: x:int -> y:int -> Content.t -> unit = fun ~x ~y c ->
    Map.data.(y).(x) <- c


  let level = ((Map.width * Map.height) / 100) + 1
  let _ = assert (100 * (level - 1) < Map.width * Map.height)
  let _ = assert (Map.width * Map.height <= 100 * level)

  module Time = struct
    let end_of_lives = 127 * Map.width * Map.height * 16

    let fruit_1_appear = 127 * 200
    let fruit_2_appear = 127 * 400
    let fruit_1_expires = 127 * 280
    let fruit_2_expries = 127 * 480
    let fright_mode_duration = 127 * 20

  end

  type lambda_man =
      {
        (* gcc: Gcc.state*)
        mutable lman_x : int;
        mutable lman_y : int;
        (* [Some start_time] means that the firght mode is active
        since start_time *)
        mutable fright_mode: int option;
        mutable lives: int;
        mutable tick_to_move: int
      }

  type ghost =
      {
        ghc: Ghc.state;
        mutable ghost_x : int;
        mutable ghost_y: int;
        mutable ghost_tick_to_move: int;
      }

  type state =
      {
        ghosts: ghost array;
        lambda_man : lambda_man;
        mutable tick: int;
        mutable pills: int;     (* remaining pills *)
      }


  let reset_positions state =
    let x,y = Map.lambda_man_start in
    state.lambda_man.lman_x <- x;
    state.lambda_man.lman_y <- y;
    Array.iteri
      (fun i ghost ->
       let x,y = Map.ghosts_start.(i) in
       state.ghosts.(i).ghost_x <- x;
       state.ghosts.(i).ghost_y <- y;
      ) state.ghosts

  let ghost_move state ghost =
    Ghc.execute (Obj.magic 1) ghost.ghc

  let lman_move state lman =
    ()

  let tick state =

    let lman = state.lambda_man in

    begin
      (* All Lambda-Man and ghost moves scheduled for this tick take
place. (Note that Lambda-Man and the ghosts do not move every tick,
only every few ticks; see the ticks section below.)  *)

      if lman.tick_to_move = state.tick
      then lman_move state lman;

      Array.iter
        (fun ghost ->
         if ghost.ghost_tick_to_move = state.tick
         then ghost_move state ghost
        ) state.ghosts;
    end;

    begin
      (* Next, any actions (fright mode deactivating, fruit *)
      (* appearing/disappearing) take place. *)
      ()
    end;
    begin
      let x = lman.lman_x in
      let y = lman.lman_y in
      match get ~x ~y with
      | Content.Pill ->
         state.pills <- state.pills - 1;
         set ~x ~y Content.Empty;
      (* If Lambda-Man occupies a square with a pill, the pill is
        eaten by Lambda-Man and removed from the game. *)
      | Content.PowerPill ->
         set ~x ~y Content.Empty;
         lman.fright_mode <- Some state.tick
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
           if lman.lman_x = ghost.ghost_x && lman.lman_y = ghost.ghost_y
           then
             begin
               (* Check what happens if two lambda-men are eaten at
                the same point in time. *)
               raise Reset_positions;
             end
          ) state.ghosts;
      with
        Reset_positions -> reset_positions state
    end;

    (* Next, if all the ordinary pills (ie not power pills) have been
eaten, then Lambda-Man wins and the game is over. *)
    if state.pills = 0
    then raise Win;

    if state.lambda_man.lives = 0
    then raise Lose;

    state.tick <- state.tick + 1

end
