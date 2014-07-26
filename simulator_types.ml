module L = struct
  type t =
      {
        mutable x : int;
        mutable y : int;
        mutable tick_to_move: int;
        mutable direction: int;
        mutable lives: int;
      }

end

module G = struct

  type t =
      {
        mutable x : int;
        mutable y: int;
        mutable tick_to_move: int;
        mutable direction: int;
        mutable vitality: int;

        index: int;      (* ghost index, between 0 and 3 included *)
        program_index: int;
      }
end

type game_state = {
  ghosts: G.t array;
  lambda_man : L.t;
  mutable tick: int;      (* UTC *)
  mutable pills: int;     (* remaining pills *)
  mutable fright_mode: int option;
    (* [Some start_time]
       means that the fright mode is active since start_time *)
}

type state = {
  game : game_state;
  mutable ghost_procs: Ghc.state array;
  ghost_codes : Ghc.code array;
  mutable lambda_proc: Gcc.registers;
  lambda_code : Gcc_instr.code;
}
