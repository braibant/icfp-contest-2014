open Simulator_types
open Simulator_constants

exception Reset_positions
exception Win
exception Lose

let free_adj board (x,y) =
  [|
    Board.get board ~x ~y:(y - 1) <> Content.Wall; (* UP *)
    Board.get board ~x:(x+1) ~y   <> Content.Wall; (* RIGHT *)
    Board.get board ~x ~y:(y + 1) <> Content.Wall; (* DOWN *)
    Board.get board ~x:(x-1) ~y   <> Content.Wall; (* LEFT *)
   |]

let move_if_free free_adj dir (x,y) =
  if free_adj.(dir)
  then match dir with
       | 0 -> (x, y-1)
       | 1 -> (x+1, y)
       | 2 -> (x, y+1)
       | 3 -> (x-1, y)
       | _ -> assert false
  else (x,y)

module L = struct
  include Simulator_types.L

  let get_move gcc_env lambda_program lambda_state lambda_step =
    let open Gcc in
    let input =
      {
        s = [
          lambda_step;   (* function to call: the step function *)
          gcc_env;       (* second argument: the state of the world *)
          lambda_state;  (* first argument: the AI state *)
        ];
        e = [];
        c = Gcc_instr.Code 0;
        d = [control_tag Stop ()];
      }
      |> ap Tail 2 (* call the step function *)
    in
    match run lambda_program input with
      | {
          s = [Value (Pair, (state, move))];
          e = _;
          c = _;
          d = [];
        } ->
	 let dir = untag Int move in
        Printf.printf "lambda-man: %i instrs to find dir %i\n%!"
		      !Gcc.nb_step dir;
        state, dir
      | _ -> failwith "gcc step failure"

  let move gcc_env lambda_code state board pos =
    let lambda_state, direction =
      get_move gcc_env
        lambda_code state.lambda_state state.lambda_step in
    state.lambda_state <- lambda_state;
    let free = free_adj board pos in
    direction, move_if_free free direction pos

  let position lman = (lman.x, lman.y)
  let set_position lman (x, y) =
    lman.x <- x;
    lman.y <- y;
    ()

  let set_direction lman dir =
    lman.direction <- dir

  let make (x,y) =
    {
      x;
      y;
      tick_to_move = 128;
      lives = 3;
      direction = 2;            (* everybody looks down at the beginning. *)
    }

  let set_next_move utc g =
    ()
end

module G = struct

  include Simulator_types.G

  (* 0 is up; 1 is right; 2 is down; 3 is left. *)

  let dreamed_move
      (environment: Ghc.env)
      (ghost: Simulator_types.G.t)
      (ghc_state: Ghc.state) =
    let direction =
      try
        Ghc.execute environment ghc_state
      with
      | e ->
         Format.printf "Uncaught ecxeption %s in GHC"
                       (Printexc.to_string e);
         None
    in
    match direction with
      | None -> ghost.direction
      | Some direction -> direction

  let position g =
      g.x, g.y

  let set_position g (x, y) =
    g.x <- x;
    g.y <- y;
    ()

  let set_direction g dir =
    g.direction <- dir

  let stats g =
    g.vitality, g.direction

  let is_frightened g =
    g.vitality = 1

  let set_next_move utc g =
    if is_frightened g
    then g.tick_to_move <- utc + Delay.ghost_fright.(g.index mod 4)
    else g.tick_to_move <- utc + Delay.ghost.(g.index mod 4)

  let make (x,y) index =
    {
      x;
      y;
      tick_to_move = Delay.ghost.(index mod 4);
      direction = 2;
      vitality = 0;
      index;
    }

  let reset ghost fright_mode (x,y) =
    ghost.x <- x;
    ghost.y <- y;
    ghost.direction <- 2;
    ghost.vitality <- match fright_mode with | None -> 0 | Some _ -> 2

  let move_restrictor board (x, y) old_direction new_direction =
    let free = free_adj board (x,y) in
    let forbidden = ((old_direction + 2) mod 4) in
    let direction =
      if List.for_all
	   (fun d -> d = forbidden || not free.(d)) [0;1;2;3]
      then forbidden
      else
	let () = free.(forbidden) <- false in
	if free.(new_direction)
	then new_direction
	else if free.(old_direction)
	then old_direction
	else List.find (fun d -> free.(d) ) [0;1;2;3]
    in
    assert free.(direction);
    (direction, move_if_free free direction (x,y))

  let move ghc_env ghost board state =
    let new_direction =
      dreamed_move ghc_env ghost state in
    let pos = position ghost in
    move_restrictor board pos ghost.G.direction new_direction

end

module Make (M : sig
                   val board : Board.t
                   val ghost_programs : Ghc.code array
                   val lambda_program : Gcc_instr.instruction array
                   val use_graphics : bool
                 end) =
struct
  include M

  let end_of_lives = 127 * Board.width board * Board.height board * 16

  let get: x:int -> y:int -> Content.t = fun ~x ~y ->
    Board.get board x y
  let set: x:int -> y:int -> Content.t -> unit = fun ~x ~y c ->
    Board.set board x y c

  let level = ((Board.width board * Board.height board) / 100) + 1
  let _ = assert (100 * (level - 1) <= Board.width board * Board.height board)
  let _ = assert (Board.width board * Board.height board <= 100 * level)

  type state = Simulator_types.state

  let reset_positions state =
    state.fright_mode <- None;
    let x,y = Board.lambda_man_start board in
    state.lambda_man.L.x <- x;
    state.lambda_man.L.y <- y;
    state.lambda_man.L.direction <- 2;
    state.lambda_man.L.lives <- state.lambda_man.L.lives - 1;
    Array.iter
      (fun ghost ->
       G.reset ghost None ((Board.ghosts_start board).(ghost.G.index)))
      state.ghosts

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

  let gcc_map = Simulator_ffi.make_gcc_map board
  let make_gcc_env game =
    Simulator_ffi.make_gcc_env gcc_map game

  let eating lman =
    let x = lman.L.x in
    let y = lman.L.y in
    match get ~x ~y with
    | Content.Pill | Content.PowerPill | Content.Fruit -> true
    | _ -> false

  let eat_a_ghost lman ghost =
    G.reset ghost (Some 0 (*dummy*))
	    ((Board.ghosts_start board).(ghost.G.index))

  let start_fright_mode game =
    Format.eprintf "Terror@.";
    Array.iter (fun ghost ->
		ghost.G.direction <- ghost.G.direction + 2 mod 4;
		ghost.G.vitality <- 1;
	       ) game.ghosts;
    game.fright_mode <-
      Some (game.tick + Time.fright_mode_duration)

  let stop_fright_mode game =
    Array.iter (fun ghost ->
		ghost.G.vitality <- 0;
	       ) game.ghosts;
    game.fright_mode <- None

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
          let gcc_env = make_gcc_env game in
          let direction, new_position =
            let pos = L.position lman in
            L.move gcc_env lambda_program state board pos in
          L.set_direction lman direction;
          L.set_position lman new_position;
          if eating lman
          then lman.L.tick_to_move <- game.tick + Delay.eating
          else lman.L.tick_to_move <- game.tick + Delay.not_eating
        end;

      let ghc_env = make_ghc_env game in
      Array.iteri
        (fun i ghost ->
          if ghost.G.tick_to_move = game.tick
          then
            begin
              Printf.printf "ghost %i to move\n" i;
              let direction, new_position =
		G.move ghc_env ghost board state.ghost_procs.(i) in
	      G.set_direction ghost direction;
              G.set_position ghost new_position;
              G.set_next_move game.tick ghost;
            end
        ) game.ghosts;
    end;

    begin match game.fright_mode with
          | None -> ()
          | Some time ->
             if time = game.tick
             then stop_fright_mode game
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
         start_fright_mode game
      | Content.Fruit ->
         set ~x ~y Content.Empty;
      | _ -> ()
    end;

    (* Next, if one or more visible ghosts are on the same square as
       Lambda-Man, then depending on whether or not fright mode is active,
       Lambda-Man either loses a life or eats the ghost(s). See below for
       details. *)
    begin
      Array.iter
        (fun ghost ->
         if lman.L.x = ghost.G.x
            && lman.L.y = ghost.G.y
         then
           begin
	     Format.printf "Beware, clash with vitality %i! @."
			   ghost.G.vitality;
             (* Check what happens if two lambda-men are eaten at
                  the same point in time. *)
             if ghost.G.vitality = 0
             then reset_positions game
             else if ghost.G.vitality = 1
             then eat_a_ghost lman ghost
             else ()
           end
        ) game.ghosts;
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
    let lambda_state, lambda_step =
      let gcc_env = make_gcc_env game in
      let open Gcc_instr in
      let open Gcc in
      let input = {
        s = [];
        e = [Unique [|
          gcc_env;   (* first argument: the state of the world *)
          tag Int 0; (* second argument: unused so far *)
        |] ];
        c = Code 0;
        d = [ control_tag Stop () ];
      }
      in
      match Gcc.run lambda_program input with
        | {
            s = [Value (Pair, (state, step))];
            e = [Unique [|_; _|]];
            c = _;
            d = [];
          } ->
          Printf.printf "lambda-man: %i instrs\n%!" !Gcc.nb_step;
          state, step
        | _ -> failwith "gcc initialization failure"
    in
    {
      game;
      ghost_procs;
      ghost_codes;
      lambda_state;
      lambda_step;
    }

  (** [repl] stands for Read-Eval-Print-Loop and is the main loop for
  our simulator.  *)
  let repl () =
    let state = init in
    if use_graphics
    then begin
      Display.init board;
      Printf.printf "Command:\n \
        p/r : step-by-step activate/deactivate\n \
        +/- : faster/slower\n \
        q: quit\n \
        _ : one step\n \
        "
    end
    else print_endline "init done";
    try
      let st_by_st = ref (true) in
      let continue = ref (true) in
      let sleep    = ref 8  in
      let react = function
	| 's' | '\n' -> ()
	| 'r' -> st_by_st := false
	| 'p' | ' ' -> st_by_st := true
	| 'q'  (*| s when int_of_char s = 27*) ->
	   continue := false
        | '+' -> sleep := if !sleep > 1000 then !sleep else !sleep * 2
        | '-' -> sleep := if !sleep = 1 then 1 else !sleep / 2
	| _ -> () in
      while !continue do
        if !st_by_st || game.tick mod 100 = 0 then
          Printf.printf "tick %d\n%!" game.tick;
        if use_graphics
        then
          begin
            Display.show board state.game;
            if game.tick mod !sleep = 0 && Graphics.key_pressed ()
	    then react (Graphics.read_key ())
	    else if !st_by_st then
	      react (Graphics.wait_next_event [Graphics.Key_pressed]).Graphics.key;
	    if not !st_by_st
	    then let () =
                     if game.tick mod !sleep = 0 then
		       try ignore(Unix.select [] [] [] 0.005)
                       with Unix.Unix_error (Unix.EINTR, _, _) -> ()
       in ();
          end;
        tick state
      done
    with
    | Win -> Format.printf "Lambda-man has won\n"
    | Lose -> Format.printf "Lambda-man has lost\n"
end
