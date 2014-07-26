
type state

type code = Ghc_instr.instr array

(** [init i c] initialize a ghost of index [i] with code [c].  *)
val init : int -> code -> state

type env = {
  lman_coordinates : (int * int) array;
  ghost_starting_positions : (int * int) array;
  ghost_current_positions : (int * int) array;
  ghost_stats : (int * int) array;
  map : Board.t;
}

(** [execute env state] is a complete execution of a ghost code. It
may return a new direction for the ghost, which is coded as an integer
in the [ 0; 3 ] range. *)
val execute : ?debug:bool -> env -> state -> int option
