
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

let load_var env x =
  let addr = MStr.find (Ident.unique_name x) env.cenv in
  LD (addr.aframe,addr.avar)


exception Not_implemented of lambda

(** [compile e] compiles program [e] into a list of machine instructions. *)
let rec compile env lambda =
  (* Format.eprintf "@[LOG:%a@]@." Printlambda.lambda lambda; *)
  match lambda with
  | Lvar x -> [load_var env x]
  (** constant block *)
  | Lconst(Const_base(Asttypes.Const_int(k))) -> [LDC k]
  | Lconst(Const_pointer(k)) -> [LDC k]
  | Lconst(Const_block(tag,l)) ->
    let l = List.map (fun x -> Lconst x) l in
    compile env (Lprim(Pmakeblock(tag,Asttypes.Immutable),l))

  | Lprim(Pmakeblock(tag,Asttypes.Immutable),l) ->
    let rec make = function
      | [] -> assert false
      | [a] -> compile env a @ [LDC 0; CONS]
      (* | [a;b] -> (compile env a) @ (compile env b) @ [CONS] *)
      | a::l  -> (compile env a) @ (make l) @ [CONS]
    in
    (LDC tag) :: (make l) @ [CONS]

  (** accessor *)
  | Lprim(Pfield(i),[a]) ->
    let rec mk_access acc i =
      if i <= 0 then List.rev (CAR::acc)
      else mk_access (CDR::acc) (i-1) in
    (compile env a) @ (mk_access [(** remove tag *) CDR] i)

  (** switch *)
  | Lswitch (Lvar(v), ({sw_failaction = None} as switch)) ->
    let v = load_var env v in
    (**  ~first first non-tail branch *)
    let rec switch_int env ~first last v = function
      | [] -> assert false
      | [_,e] -> compile env e@[last]
      | (i,e)::l ->
        let e = save_instrs env (compile env e@[last]) in
        let l = save_instrs env (switch_int env ~first:false last v l) in
        v::LDC(i)::CEQ::(if first then [SEL(e,l)] else [TSEL(e,l)]) in
    let mk_const ~first = switch_int env ~first JOIN v switch.sw_consts in
    let mk_block () =
      let env = shift_frame env in
      let tag = (LD(0,0)) in
      let l = switch.sw_blocks in
      let l = save_instrs env (switch_int env ~first:false RTN tag l) in
      [v;CAR;LDF(l);AP(1)]
    in
    begin match switch.sw_consts, switch.sw_blocks with
      | [], [] -> assert false
      | _, [] -> mk_const ~first:true
      | [], _ -> mk_block ()
      | _, _ ->
        let lint = save_instrs env (mk_const ~first:false) in
        let lblock = save_instrs env (mk_block ()@[JOIN]) in
        [v;ATOM;SEL(lint,lblock)]
    end

  (** primitive arithmetic *)
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

  (** controlflow *)
  | Lifthenelse (e1, e2, e3) ->
    let e2 = save_instrs env (compile env e2@[JOIN]) in
    let e3 = save_instrs env (compile env e3@[JOIN]) in
    (** if can be used on any type, so we call LDF(3) *)
    (compile env e1) @ [LDF(Code 3);AP(1)] @ [SEL(e2,e3)]
  | Lletrec (fs, e) ->
    let env = shift_frame env in
    let env,n = prepare_env env 0 (List.map fst fs) in
    let rec save_fs env = function
      | [] ->
        let e = save_instrs env (compile env e@[RTN]) in
        [LDF e; RAP n]
      | (_,e)::l ->
        let e = compile env e in
        (match e with [LDF _] -> () | _ -> raise (Not_implemented lambda));
        e@(save_fs env l)
    in
    (DUM n)::save_fs env fs
  | Lfunction (Curried, args, e) ->
    let env = shift_frame env in
    let env,_n = prepare_env env 0 args in
    let e = save_instrs env (compile env e@[RTN]) in
    [LDF e]
  | Lapply (e1, args, _) ->
    (List.concat (List.map (compile env) args)) @ (compile env e1) @
    [AP (List.length args)]
  (** stop compiling at main_gcc *)
  | Llet(_,main,e1,_) when main.Ident.name = "main_gcc"  ->
    compile env e1
  | Llet(_,x,e1,e2) ->
    compile env (Lapply(Lfunction(Curried,[x],e2),[e1],Location.none))
  (** GCC primitive *)
  | Lprim(Pccall({prim_name="gcc_left"}),[i]) ->
    compile env i
  | Lprim(Pccall({prim_name="gcc_right"}),[a;b]) ->
    (compile env a)@(compile env b)@[CONS]
  | Lprim(Pccall({prim_name="gcc_case"}),[l;left;right]) ->
    let run_left = save_instrs env  [LD(0,0);LD(0,1);TAP(1)] in
    let run_right = save_instrs env [LD(0,0);CAR;LD(0,0);CDR;LD(0,2);TAP(2)] in
    let call = save_instrs env [LD(0,0);ATOM;TSEL(run_left,run_right)] in
    (compile env l) @ (compile env left) @ (compile env right) @
    [LDF(call);AP(3)]
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
  let env =  {cenv = MStr.empty; slots = {sinstr=[];snext=10}} in
  let main = save_instrs env ((compile env expr)@[RTN]) in
  (* 0 *) (LDF main)::
          (* 1 *)(AP 0)::
          (* 2 *)STOP::
          (** function is not null *)
          (* 3 *) LD(0,0)::
          (* 4 *) ATOM::
          (* 5 *) TSEL(Code 6,Code 8)::
          (* 6 *) LD(0,0)::
          (* 7 *) RTN::
          (* 8 *) LDC(1)::
          (* 9 *) RTN ::
          (* 10 *) (get_slots env)


let compile_implementation _modulename (e:Lambda.lambda) =
  compile e
