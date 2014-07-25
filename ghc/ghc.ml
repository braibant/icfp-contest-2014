(* CONSTANTS *)
let size = 255
let nreg= 8


exception Not_lvalue
exception Halt

module Byte = struct
  type t = int 			(* 0 to 255 *)
  let succ i = (i + 1) mod 256
  let pred i = (i - 1) mod 256
  let add  i j = (i + j) mod 256
  let sub i j = (i - j) mod 256
  let mul i j = (i * j) mod 256
  let div i j = (i / j) mod 256
  let _land i j = i land j
  let _lor i j = i lor j
  let _lxor i j = i lxor j

  let lt i j = i < j
  let eq i j = i = j
  let gt i j = i > j
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
  | JUMP of comparison * byte * argument * argument

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

(** [set state dest value] updates the lvalue [dest] in [state] to
hold the value [value]. *)
let set state dest value =
  match dest with
  | Register reg -> state.registers.(reg) <- value
  | PC -> raise Not_lvalue
  | Indirect reg -> state.data.(state.registers.(reg)) <- value
  | Constant _ -> raise Not_lvalue
  | Location cst -> state.data.(cst) <- value

type interrupts =
    {
      set_ghost_direction: byte -> unit;          (* INT 0 *)
      get_lman1_coordinates: unit -> byte * byte; (* INT 1 *)
      get_lman2_coordinates: unit -> byte * byte; (* INT 2 *)
      get_ghost_index: unit -> byte;		  (* INT 3 *)
      get_ghost_starting_coordinates: byte -> byte * byte;  (* INT 4 *)
      get_ghost_current_coordinates: byte -> byte * byte;  (* INT 5 *)
      get_ghost_current_stats: byte -> byte * byte;  (* INT 6 *)
      get_map_content: (byte * byte) -> byte;		 (* INT 7 *)
      debug: byte * byte array -> unit (* INT 8 *)
    }

let execution_cycle interrupts state =
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
	   | AND -> set state dest (Byte._land a b)
	   | OR -> set state dest  (Byte._lor a b)
	   | XOR -> set state dest (Byte._lxor a b)
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

  | INT int ->
     begin match int with
	   | 0 -> interrupts.set_ghost_direction state.registers.(0)
	   | 1 ->
	      let x,y = interrupts.get_lman1_coordinates () in
	      state.registers.(0) <- x;
	      state.registers.(1) <- y
	   | 2 ->
	      let x,y = interrupts.get_lman2_coordinates () in
	      state.registers.(0) <- x;
	      state.registers.(1) <- y
	   | 3 -> state.registers.(0) <- interrupts.get_ghost_index ()
	   | 4 ->
	      let a = state.registers.(0) in
	      let (x,y) = interrupts.get_ghost_starting_coordinates a in
	      state.registers.(0) <- x;
	      state.registers.(1) <- y
	   | 5 ->
 	      let a = state.registers.(0) in
	      let (x,y) = interrupts.get_ghost_current_coordinates a in
	      state.registers.(0) <- x;
	      state.registers.(1) <- y
	   | 6 ->
 	      let a = state.registers.(0) in
	      let (x,y) = interrupts.get_ghost_current_stats a in
	      state.registers.(0) <- x;
	      state.registers.(1) <- y
	   | 7 ->
	      let a = state.registers.(0) in
	      let b = state.registers.(1) in
	      let content = interrupts.get_map_content (a,b) in
	      state.registers.(0) <- content
	   | 8 ->
	      interrupts.debug (state.pc,state.registers)
	   | _ -> assert false
     end
  | HLT -> raise Halt
