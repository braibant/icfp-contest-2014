type argument =
  | Register of int 		(* A to H *)
  | PC  			(* special purpose register *)
  | Indirect of int 		(* [A] to [H] *)
  | Constant of int 		(* 0 to 255 *)
  | Location of int 		(* [0] to [255] *)



type instr = 
  | MOV of argument * argument

  (* Math *)
  | INC of argument
  | DEC of argument
  | ADD of argument * argument
  | SUB of argument * argument
  | MUL of argument * argument
  | DIV of argument * argument
  
  (* Bitwise logical operations *)
  | AND of argument * argument
  | OR of argument * argument
  | XOR of argument * argument

  (* Branch instructions *)
  | JLT of constant * argument * argument
  | JEQ of constant * argument * argument
  | JGT of constant * argument * argument

  | INT of int 

  | HLT

