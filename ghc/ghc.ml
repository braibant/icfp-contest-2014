(* CONSTANTS *)
let size = 255
let nreg= 8 

module Byte = struct
  type t = int 
end

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
  | JUMP of comparison * constant * argument * argument

  | INT of int 

  | HLT


type state = 
  {
    registers: byte array;
    data: byte array; 
    code: instr array;
    mutable pc: byte   
  }

	    
let init code = 
  let data = Array.create size 0 in 
  let registers = Array.create nreg 0 in 
  {data; registers; code; pc = 0}

let rvalue state argument = 
  match argument with 
  | Register reg -> state.registers.(reg)
  | PC           -> state.pc
  | Indirect reg -> state.data.(state.registers.(reg))
  | Constant cst -> cst
  | Location cst -> state.data.(cst)

exception Not_lvalue

(** [set state dest value] updates the lvalue [dest] in [state] to
hold the value [value]. *)
let set state dest value = 
  match dest with 
  | Register reg -> state.registers.(reg) <- value
  | PC -> raise Not_lvalue
  | Indirect reg -> state.data.(state.registers.(reg)) <- value
  | Constant _ -> raise Not_lvalue
  | Location cst -> state.data.(cst) <- value

let execution_cycle state = 
  let instr = state.code.(state.pc) in 
  match instr with
  | MOV (dest, src) -> 
     let value = rvalue state src in 
     set state dest value

  | INC dest ->
     let value = rvalue state dest in 
     set state dest (Byte.succ value)
  | DEC dest ->
     let value = rvalue state dest in 
     set state dest (Byte.pred value)
	 
  | BINOP (binop, dest, src) -> 
     assert (dest <> PC);
     let a = rvalue state dest in 
     let b = rvalue state src in 
     begin match binop with 
	   | ADD -> set state dest (Byte.add a b)
	   | SUB -> set state dest (Byte.sub a b)
	   | MUL -> set state dest (Byte.mul a b)
	   | DIV -> set state dest (Byte.div a b)
	   | AND -> set state dest (Byte.land a b)
	   | OR -> set state dest  (Byte.lor a b)
	   | XOR -> set state dest (Byte.lxor a b)
     end
       
  (* Branch instructions *)
  | JUMP (comparison,targ,x,y) ->
     let x = rvalue state x in 
     let y = rvalue state y in 
     let test = 
       match comparison with 
       | LT -> Byte.lt x y 
       | EQ -> Byte.eq x y 
       | GT -> Byte.gt x y 
     in 
     if test 
     then state.pc <- targ

  | INT i -> assert false

  | HLT -> raise Halt
			       
