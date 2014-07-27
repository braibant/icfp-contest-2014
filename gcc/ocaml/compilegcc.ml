
open Lambda
open Gcc_instr

module MStr = Map.Make(String)
module MInt = Map.Make(struct type t = int let compare = compare end)

type address = {aframe: int; (* in which frame *)
                avar  : int;}
type cenv = address MStr.t

type slots = {
  mutable snext: int; (** next instruction slot free *)
  mutable sinstr: instruction list list; (** in reverse order *)
}

type env = {
  slots: slots;
  cenv: cenv;
  scatch: address MInt.t
}

let save_instrs ?lambda env instrs =
  let len = List.length instrs in
  let slot = env.slots.snext in
  let instrs = match lambda with
    | Some lambda when !Gcc_instr.print_comment ->
      COMMENT(Pp.string_of_wnl Printlambda.lambda lambda)::instrs
    | _ -> instrs in
  env.slots.sinstr <- instrs::env.slots.sinstr;
  env.slots.snext <- env.slots.snext + len;
  Code slot

let get_slots env =
  List.concat (List.rev env.slots.sinstr)

let shift_frame env =
  let map addr = {addr with aframe = addr.aframe + 1} in
  let cenv = MStr.map map env.cenv in
  let scatch = MInt.map map env.scatch in
  {env with cenv = cenv; scatch = scatch}

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

type error =
  | Tag_not_null
  | Field_access_invalid
  | Empty_block

let print_error fmt = function
  | Tag_not_null -> Format.pp_print_string fmt "tag not null"
  | Field_access_invalid -> Format.pp_print_string fmt  "field access invalid"
  | Empty_block -> Format.pp_print_string fmt  "empty block"

exception Not_implemented of lambda
exception Error of error * lambda

(** stdlib *)
let simu_world = Ident.create " Simu.world "
let simu_ghost = Ident.create " Simu.ghost "

(** [compile e] compiles program [e] into a list of machine instructions. *)
let rec compile env lambda =
  (* Format.eprintf "@[LOG:%a@]@." Printlambda.lambda lambda; *)
  match lambda with
  | Lvar x -> [load_var env x]
  (** constant block *)
  | Lconst(Const_base(Asttypes.Const_int(k))) -> [LDC k]
  | Lconst(Const_pointer(k)) -> [LDC k]
  | Lconst(Const_block(tag,l)) ->
    if tag != 0 then raise (Error(Tag_not_null,lambda));
    let l = List.map (fun x -> Lconst x) l in
    compile env (Lprim(Pmakeblock(tag,Asttypes.Immutable),l))

  | Lprim(Pmakeblock(_,Asttypes.Immutable),[]) ->
    raise (Error(Empty_block,lambda))

  | Lprim(Pmakeblock(tag,Asttypes.Immutable),l) ->
    if tag != 0 then raise (Error(Tag_not_null,lambda));
    let rec make = function
      | [] -> assert false
      | [a] -> compile env a @ [LDC 0; CONS]
      | [a;b] -> (compile env a) @ (compile env b) @ [CONS]
      | a::l  -> (compile env a) @ (make l) @ [CONS]
    in
    (make l)

  | Lprim(Pisint,[a]) -> (compile env a) @ [ATOM]

  (** accessor *)
  | Lprim(Pfield(0),[Lprim(Pgetglobal(modid),[])])
    when modid.Ident.name = "Simu" ->
    [load_var env simu_world]

  | Lprim(Pfield(1),[Lprim(Pgetglobal(modid),[])])
    when modid.Ident.name = "Simu" ->
    [load_var env simu_ghost]

  | Lprim(Pfield(0),[a]) -> (compile env a) @ [CAR]
  | Lprim(Pfield(1),[a]) -> (compile env a) @ [CDR]
  | Lprim(Pfield(_),[_]) ->
    raise (Error(Field_access_invalid,lambda))

  (** switch *)
  | Lswitch (Lvar(v), ({sw_failaction = None} as switch)) ->
    let v = load_var env v in
    (**  ~first first non-tail branch *)
    let rec switch_int env ~first last v = function
      | [] -> assert false
      | [_,e] -> compile env e@[last]
      | (i,e)::l ->
        let e = save_instrs ~lambda:e env (compile env e@[last]) in
        let l = save_instrs env (switch_int env ~first:false last v l) in
        v::LDC(i)::CEQ::(if first then [SEL(e,l)] else [TSEL(e,l)]) in
    let mk_const ~first = switch_int env ~first JOIN v switch.sw_consts in
    (* let mk_block () = *)
    (*   let env = shift_frame env in *)
    (*   let tag = (LD(0,0)) in *)
    (*   let l = switch.sw_blocks in *)
    (*   let l = save_instrs env (switch_int env ~first:false RTN tag l) in *)
    (*   [v;CAR;LDF(l);AP(1)] *)
    (* in *)
    begin match switch.sw_consts, switch.sw_blocks with
      | [], [] -> assert false (** nothing, no switch *)
      | _, [] -> mk_const ~first:true
      | _, _::_::_ -> raise (Error(Tag_not_null,lambda))
      | [], [_] -> assert false (** one case? no switch *)
      | _, [_] ->
        assert false (** TODO: I though that only use isint in this case *)
        (* let lint = save_instrs env (mk_const ~first:false) in *)
        (* let lblock = save_instrs env (mk_block ()@[JOIN]) in *)
        (* [v;ATOM;SEL(lint,lblock)] *)
    end

  (** static raise *)
  | Lstaticcatch(lbody,(i,vars), lhandler) ->
    let env' = shift_frame env in
    let env',_ = prepare_env env' 0 vars in
    let lhandler = save_instrs ~lambda:lhandler env'
        (compile env' lhandler@[RTN]) in
    let env = shift_frame env in
    let env = {env with scatch = MInt.add i {aframe=0;avar=0} env.scatch} in
    let lbody = save_instrs ~lambda:lbody env (compile env lbody@[RTN]) in
    (LDF lhandler)::(LDF lbody)::[AP(1)]

  | Lstaticraise(i,args) ->
    let lhandler = MInt.find i env.scatch in
    (List.concat (List.map (compile env) args)) @
    [LD(lhandler.aframe,lhandler.avar);
     AP (List.length args)]

  (** primitive arithmetic *)
  | Lprim(Pintcomp(Ceq),[a;Lconst(Const_pointer(0))]) ->
    (compile env a)@[ATOM]

  | Lprim(Pmulint|Psubint|Paddint|Pdivint|Pintcomp(Ceq|Cgt|Cge)
          as prim,[e1;e2]) ->
    (compile env e1) @ (compile env e2) @
    [match prim with
     | Pmulint -> MUL | Psubint -> SUB | Paddint -> ADD | Pdivint -> DIV
     | Pintcomp(Ceq) -> CEQ | Pintcomp(Cgt) -> CGTE | Pintcomp(Cge) -> CGT
     | _ -> assert false]

  | Lprim(Pnegint,[a]) -> (LDC 0)::(compile env a)@[SUB]

  | Lprim(Pintcomp(Cneq),args) ->
    compile env (Lprim(Psubint,[Lconst(Const_base(Const_int(1)));
                                Lprim(Pintcomp(Ceq),args)]))
  | Lprim(Pintcomp(Clt),[a1;a2]) ->
    compile env (Lprim(Pintcomp(Cgt),[a2;a1]))
  | Lprim(Pintcomp(Cle),[a1;a2]) ->
    compile env (Lprim(Pintcomp(Cge),[a2;a1]))
  | Lprim(Psetglobal _,[e]) -> compile env e

  | Lprim(Pisout,[h;arg]) -> (* isout is lt ? *)
    let isout = save_instrs env
        [LD(0,0);LD(0,1);CGT;LDC 0;LD(0,0);CGT;ADD;RTN] in
    (compile env arg)@(compile env h)@[LDF isout; AP 2]

  | Lprim(Poffsetint(i),[arg]) ->
    (compile env arg)@[LDC i;ADD]

  (** primitive boolean *)
  | Lprim(Psequand,[a1;a2]) ->
    let a2 = save_instrs env (compile env a2@[JOIN]) in
    let false_ = save_instrs env ([LDC 0;JOIN]) in
    (compile env a1)@[SEL(a2,false_)]
  | Lprim(Psequor,[a1;a2]) ->
    let a2 = save_instrs env (compile env a2@[JOIN]) in
    let true_ = save_instrs env ([LDC 1;JOIN]) in
    (compile env a1)@[SEL(true_,a2)]
  | Lprim(Pnot,[a1]) ->
    let false_ = save_instrs env ([LDC 0;JOIN]) in
    let true_ = save_instrs env ([LDC 1;JOIN]) in
    (compile env a1)@[SEL(false_,true_)]


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
  (** stop compiling at main_ia;
      we use a different calling convention: main_ia is of the form
        let main_ia world ghosts = ...
      but we don't want to return the closure,
      rather directly run the function body
  *)
  | Llet(_,main,e1,_) when main.Ident.name = "main_ia"  ->
    begin match e1 with
      | Lfunction(Curried, [world;ghosts], body) ->
        let env,_n = prepare_env env 0 [world;ghosts] in
        compile env body
      | _ ->
        failwith "invalid main_ia shape: \
                  two currified arguments expected"
    end
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
  (** exception *)
  | Lprim(Praise,_) -> [BRK]
  | e -> raise (Not_implemented e)

let compile expr =
  let env =  {cenv = MStr.empty; slots = {sinstr=[];snext=10};
              scatch = MInt.empty} in
  let env,_ = prepare_env env 0 [simu_world;simu_ghost] in
  let env = shift_frame env in
  let main = save_instrs env ((compile env expr)@[RTN]) in
  LDF(main):: (* 0 *)
  AP(0):: (* 1 *)
  RTN:: (* 2 *)
  (** function is not null *)
  LD(0,0):: (* 3 *)
  ATOM:: (* 4 *)
  TSEL(Code 6,Code 8):: (* 5 *)
  LD(0,0):: (* 6 *)
  RTN:: (* 7 *)
  LDC(1):: (* 8 *)
  RTN :: (* 9 *)
  (get_slots env)


let compile_implementation _modulename (e:Lambda.lambda) =
  compile e
