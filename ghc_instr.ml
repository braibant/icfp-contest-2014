type byte = Byte.t

type argument =
  | Register of int 		(* A to H, i.e., 0 to 7 *)
  | PC  			(* special purpose register *)
  | Indirect of int		(* [A] to [H], i.e., [0] to [7]*)
  | Constant of byte 		(* 0 to 255 *)
  | Location of byte 		(* [0] to [255] *)


type binop =
  | ADD
  | SUB
  | MUL
  | DIV
  | AND
  | OR
  | XOR

type comparison =
  | LT | EQ | GT

type instr =
  | MOV of argument * argument

  (* Math *)
  | INC of argument
  | DEC of argument
  | BINOP of binop * argument * argument

  (* Branch instructions *)
  | JUMP of comparison * byte * argument * argument

  | INT of int

  | HLT
