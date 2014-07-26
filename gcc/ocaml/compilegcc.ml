
open Lambda
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
  {env with cenv = MStr.add (Ident.unique_name x) addr env.cenv}

let rec prepare_env env n = function
  | [] -> env,n
  | f::l ->
    let env = add_frame env f {aframe=0;avar=n} in
    prepare_env env (n+1) l

exception Not_implemented of lambda

(** [compile e] compiles program [e] into a list of machine instructions. *)
let rec compile env lambda =
  (* Format.eprintf "@[LOG:%a@]@." Printlambda.lambda lambda; *)
  match lambda with
  | Lvar x ->
    let addr = MStr.find (Ident.unique_name x) env.cenv in
    [LD (addr.aframe,addr.avar)]
  | Lconst(Const_base(Const_int(k))) -> [LDC k]
  | Lprim(Pmulint|Psubint|Paddint|Pintcomp(Ceq|Cgt|Cge)
          as prim,[e1;e2]) ->
    (compile env e1) @ (compile env e2) @
    [match prim with
     | Pmulint -> MUL | Psubint -> SUB | Paddint -> ADD
     | Pintcomp(Ceq) -> CEQ | Pintcomp(Cgt) -> CGTE | Pintcomp(Cge) -> CGT
     | _ -> assert false]
  | Lprim(Pintcomp(Cneq),args) ->
    compile env (Lprim(Psubint,[Lconst(Const_base(Const_int(1)));
                                Lprim(Pintcomp(Ceq),args)]))
  | Lprim(Pintcomp(Clt),[a1;a2]) ->
    compile env (Lprim(Pintcomp(Cgt),[a2;a1]))
  | Lprim(Pintcomp(Cle),[a1;a2]) ->
    compile env (Lprim(Pintcomp(Cge),[a2;a1]))
  | Lprim(Psetglobal _,[e]) -> compile env e
  | Lifthenelse (e1, e2, e3) ->
    let e2 = save_instrs env (compile env e2) in
    let e3 = save_instrs env (compile env e3) in
    (compile env e1) @ [SEL(e2,e3)]
  | Lletrec (fs, e) ->
    let env = shift_frame env in
    let env,n = prepare_env env 0 (List.map fst fs) in
    let rec save_fs env = function
      | [] ->
        let e = save_instrs env (compile env e@[RTN]) in
        [LDF e; RAP n]
      | (_,e)::l ->
        let e = save_instrs env (compile env e@[RTN]) in
        (LDF e)::(save_fs env l)
    in
    (DUM n)::save_fs env fs
  | Lfunction (Curried, args, e) ->
    let env = shift_frame env in
    let env,_n = prepare_env env 0 args in
    let e = save_instrs env (compile env e@[RTN]) in
    [LDF e]
  | Lapply (e1, args, _) ->
    (List.concat (List.map (compile env) args)) @ (compile env e1) @ [AP 1]
  (** stop compiling at main_gcc *)
  | Llet(_,main,e1,_) when main.Ident.name = "main_gcc"  ->
    compile env e1
  | Llet(_,x,e1,e2) ->
    compile env (Lapply(Lfunction(Curried,[x],e2),[e1],Location.none))
  | Lprim(Pmakeblock(tag,Immutable),l) ->
    let rec make = function
      | [] -> assert false
      | [a] -> compile env a
      | [a;b] -> (compile env a) @ (compile env b) @ [CONS]
      | a::l  -> (compile env a) @ (make l) @ [CONS]
    in
    (LDC tag) :: (make l) @ [CONS]
  | e -> raise (Not_implemented e)

(*
let builtin_match_list_alpha =
  let env = shift_frame env in
  let env = add_frame env "l"     {aframe=0;avar=0} in
  let env = shift_frame env in
  let env = add_frame env "fint"  {aframe=0;avar=0} in
  let env = shift_frame env in
  let env = add_frame env "flist" {aframe=0;avar=0} in
  let fint_call = compile env [APPLY(Var "fint",Var "l") in
  let fint_call = save_instrs env fint_call in
  let flist_call = (compile env [Var "flist"])@[AP 2] in
  let flist_call = save_instrs env fint_call in
  (compile env (Var l))@[ATOM]@[TSEL(fint_call,flist_call)]
*)

let compile expr =
  let env =  {cenv = MStr.empty; slots = {sinstr=[];snext=3}} in
  let main = save_instrs env ((compile env expr)@[RTN]) in
  (LDF main)::(AP 0)::STOP::(get_slots env)


let compile_implementation _modulename (e:Lambda.lambda) =
  compile e
