(* CONSTANTS *)
let size = 255
let nreg= 8


exception Not_lvalue
exception Halt


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
    mutable pc: byte;

    index: int
  }


let init index code =
  let data = Array.create size 0 in
  let registers = Array.create nreg 0 in
  {data; registers; code; pc = 0; index}

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


type env =
    {
      lman_coordinates: (int * int) array;
      ghost_starting_positions: (int * int) array;
      ghost_current_positions : (int * int) array;
      ghost_stats : (int * int) array;
      map: Content.t array array;

      set_direction: byte -> unit;
      debug: byte * byte array -> unit
    }

let execution_cycle env state =
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
	   | 0 -> env.set_direction state.registers.(0)
	   | 1 ->
	      let x,y = env.lman_coordinates.(0) in
	      state.registers.(0) <- x;
	      state.registers.(1) <- y
	   | 2 ->
	      let x,y = env.lman_coordinates.(1) in
	      state.registers.(0) <- x;
	      state.registers.(1) <- y
	   | 3 ->
              state.registers.(0) <- state.index
	   | 4 ->
	      let a = state.registers.(0) in
	      let (x,y) = env.ghost_starting_positions.(a) in
	      state.registers.(0) <- x;
	      state.registers.(1) <- y
	   | 5 ->
 	      let a = state.registers.(0) in
	      let (x,y) = env.ghost_current_positions.(a) in
	      state.registers.(0) <- x;
	      state.registers.(1) <- y
	   | 6 ->
 	      let a = state.registers.(0) in
	      let (x,y) = env.ghost_stats.(a) in
	      state.registers.(0) <- x;
	      state.registers.(1) <- y
	   | 7 ->
	      let x = state.registers.(0) in
	      let y = state.registers.(1) in
	      let content = Map.get env.map ~x ~y in
	      state.registers.(0) <- Content.to_byte content
	   | 8 ->
	      env.debug (state.pc,state.registers)
	   | _ -> assert false
     end
  | HLT -> raise Halt


let execute interrupts state =
  let i = ref 0 in
  try
    state.pc <- 0;
    while !i < 1024 do
      let pc = state.pc in
      execution_cycle interrupts state;
      if state.pc = pc then state.pc <- state.pc + 1
    done
  with Halt -> ()
