(** MiniML compiler. *)

open Syntax
open Machine

module MStr = Map.Make(String)

type address = {aframe: int; (* in which frame *)
                avar  : int;}
type cenv = address MStr.t


(** [compile e] compiles program [e] into a list of machine instructions. *)
let rec compile = function
  | Var x -> [IVar x]
  | Int k -> [IInt k]
  | Bool b -> [IBool b]
  | Times (e1, e2) -> (compile e1) @ (compile e2) @ [IMult]
  | Plus (e1, e2) -> (compile e1) @ (compile e2) @ [IAdd]
  | Minus (e1, e2) -> (compile e1) @ (compile e2) @ [ISub]
  | Equal (e1, e2) -> (compile e1) @ (compile e2) @ [IEqual]
  | Less (e1, e2) -> (compile e1) @ (compile e2) @ [ILess]
  | If (e1, e2, e3) -> (compile e1) @ [IBranch (compile e2, compile e3)]
  | FunRec (f, x, _, _, e) -> [IClosure (f, x, compile e @ [IPopEnv])]
  | Fun (x, _, e) -> [IClosure (" dumb", x, compile e @ [IPopEnv])]
  | Apply (e1, e2) -> (compile e1) @ (compile e2) @ [ICall]
