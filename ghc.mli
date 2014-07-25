
type byte = Byte.t

type argument =
    Register of int
  | PC
  | Indirect of int
  | Constant of byte
  | Location of byte

type binop = ADD | SUB | MUL | DIV | AND | OR | XOR
type comparison = LT | EQ | GT
type instr =
    MOV of argument * argument
  | INC of argument
  | DEC of argument
  | BINOP of binop * argument * argument
  | JUMP of comparison * byte * argument * argument
  | INT of int
  | HLT

type state

(** [init i c] initialize a ghost of index [i] with code [c].  *)
val init : int -> instr array -> state

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
val execute : env -> state -> int option
