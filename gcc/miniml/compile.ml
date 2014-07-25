(** MiniML compiler. *)

open Syntax
open Gcc_instr

module MStr = Map.Make(String)

type address = {aframe: int; (* in which frame *)
                avar  : int;}
type cenv = address MStr.t

type slots = {
  mutable snext: int; (** next instruction slot free *)
  mutable sinstr: instruction list list; (** in reverse order *)
}

type env = {
  slots: slots;
  cenv: cenv
}

let save_instrs env instrs =
  let len = List.length instrs in
  let slot = env.slots.snext in
  env.slots.sinstr <- instrs::env.slots.sinstr;
  env.slots.snext <- env.slots.snext + len;
  Code slot

let get_slots env =
  List.concat (List.rev env.slots.sinstr)

let shift_frame env =
  let map addr = {addr with aframe = addr.aframe + 1} in
  let cenv = MStr.map map env.cenv in
  {env with cenv = cenv}

let add_frame env x addr =
  {env with cenv = MStr.add x addr env.cenv}

(** [compile e] compiles program [e] into a list of machine instructions. *)
let rec compile env = function
  | Var x ->
    let addr = MStr.find x env.cenv in
    [LD (addr.aframe,addr.avar)]
  | Int k -> [LDC k]
  | Bool b -> [LDC (if b then 1 else 0)]
  | Times (e1, e2) -> (compile env e1) @ (compile env e2) @ [MUL]
  | Plus (e1, e2) -> (compile env e1) @ (compile env e2) @ [ADD]
  | Minus (e1, e2) -> (compile env e1) @ (compile env e2) @ [SUB]
  | Equal (e1, e2) -> (compile env e1) @ (compile env e2) @ [CEQ]
  | GT (e1, e2) -> (compile env e1) @ (compile env e2) @ [CGT]
  | GTE (e1, e2) -> (compile env e1) @ (compile env e2) @ [CGTE]
  | If (e1, e2, e3) ->
    let e2 = save_instrs env (compile env e2) in
    let e3 = save_instrs env (compile env e3) in
    (compile env e1) @ [SEL(e2,e3)]
  | FunRec (_f, _x, _, _, _e) -> invalid_arg "not implemented"
  | Fun (x, _, e) ->
    let env = shift_frame env in
    let env = add_frame env x {aframe=0;avar=0} in
    let e = save_instrs env (compile env e) in
    [LDF e]
  | Apply (e1, e2) ->
    (compile env e1) @ (compile env e2) @ [AP 1]

let compile expr =
  let env =  {cenv = MStr.empty; slots = {sinstr=[];snext=2}} in
  let main = save_instrs env ((compile env expr)@[RTN]) in
  (LDF main)::(AP 0)::RTN::(get_slots env)
