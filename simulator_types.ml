module type MAP =
  sig
    val width : int
    val height: int
    val data: Map.t
    val lambda_man_start: int * int
    val ghosts_start: (int * int) array
  end

module L = struct
  type t =
      {
        (* gcc: Gcc.state*)
        mutable x : int;
        mutable y : int;
        mutable tick_to_move: int;
        mutable lives: int;
      }

end

module G = struct

  type t =
      {
        ghc: Ghc.state;
        mutable x : int;
        mutable y: int;
        mutable tick_to_move: int;
        mutable direction: int;
        mutable vitality: int;

        index: int;      (* ghost index, between 0 and 3 included *)
        program_index: int;
      }
end


type state =
    {
      ghosts: G.t array;
      lambda_man : L.t;
      mutable tick: int;      (* UTC *)
      mutable pills: int;     (* remaining pills *)
      mutable fright_mode: int option;      (* [Some start_time]
        means that the fright mode is active since start_time *)
    }
