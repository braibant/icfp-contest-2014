(* CONSTANTS *)
let size = 255
let nreg= 8


exception Not_lvalue
exception Halt

open Ghc_instr

type state =
  {
    registers: byte array;
    data: byte array;
    code: instr array;
    mutable pc: byte;

    index: int;
    mutable direction: byte option
  }

let pp_registers fmt registers =
  for i = 0 to Array.length registers - 1 do
    Format.fprintf fmt " %c:%.3i;"(char_of_int (int_of_char 'A' + i)) registers.(i)
  done

let pp_direction fmt =
  function
  | None -> Format.fprintf fmt "*"
  | Some 0 -> Format.fprintf fmt "^"
  | Some 1 -> Format.fprintf fmt ">"
  | Some 2 -> Format.fprintf fmt "v"
  | Some _ -> Format.fprintf fmt "<"

let pp_short fmt state =
  Format.fprintf fmt "regs [%a] pc %.3i {%a}" pp_registers state.registers state.pc pp_direction state.direction

type code = instr array

let set_direction state direction =
  state.direction <- Some direction

let init index code =
  let data = Array.create size 0 in
  let registers = Array.create nreg 0 in
  {data; registers; code; pc = 0; index; direction = None}

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
  | PC -> state.pc <- value
  | Indirect reg -> state.data.(state.registers.(reg)) <- value
  | Constant _ -> raise Not_lvalue
  | Location cst -> state.data.(cst) <- value


type env =
    {
      lman_coordinates: (int * int) array;
      ghost_starting_positions: (int * int) array;
      ghost_current_positions : (int * int) array;
      ghost_stats : (int * int) array;
      map: Board.t;
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
	   | 0 -> set_direction state state.registers.(0)
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
	      let content = Board.get env.map ~x ~y in
	      state.registers.(0) <- Content.to_byte content
	   | 8 -> Printf.eprintf "Debug\n"
	   | _ -> assert false
     end
  | HLT -> raise Halt


let execute ?(debug=false) interrupts state =
  let i = ref 0 in
  begin
    try
      state.pc <- 0;
      state.direction <- None;
      while !i < 1024 do
        let pc = state.pc in
        if debug
        then
          begin
            Format.printf "state:%a\n" pp_short state;
            Format.printf "instr: %a\n" Ghc_misc.pp_instr state.code.(state.pc);
          end;
        execution_cycle interrupts state;


        if state.pc = pc then state.pc <- state.pc + 1
      done
    with Halt -> ()
  end;

  match state.direction with
  | Some i when 0 <= i && i < 4 -> Some i
  | None | Some _ -> None
