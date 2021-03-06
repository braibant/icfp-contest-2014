(** Type checking. *)

open Syntax

(** Exception indicating a type error. *)
exception Type_error of string

(** [ty_error msg] reports a type error. *)
let type_error msg = raise (Type_error msg)

(** [check ctx ty e] verifies that expression [e] has type [ty] in
    context [ctx]. If it does, it returns unit, otherwise it raises the
    [Type_error] exception. *)
let rec check ctx ty e =
  let ty' = type_of ctx e in
    if ty' <> ty then
      type_error
	(string_of_expr e ^ " has type " ^ string_of_type ty' ^
	   " but is used as if it has type " ^ string_of_type ty)

(** [type_of ctx e] computes the type of expression [e] in context
    [ctx]. If [e] does not have a type it raises the [Type_error]
    exception. *)
and type_of ctx = function
    Var x ->
      (try List.assoc x ctx with
	   Not_found -> type_error ("unknown variable " ^ x))
  | Int _ -> TInt
  | Bool _ -> TBool
  | Times (e1, e2) -> check ctx TInt e1 ; check ctx TInt e2 ; TInt
  | Plus (e1, e2) -> check ctx TInt e1 ; check ctx TInt e2 ; TInt
  | Minus (e1, e2) -> check ctx TInt e1 ; check ctx TInt e2 ; TInt
  | Equal (e1, e2) -> check ctx TInt e1 ; check ctx TInt e2 ; TBool
  | GT (e1, e2) -> check ctx TInt e1 ; check ctx TInt e2 ; TBool
  | GTE (e1, e2) -> check ctx TInt e1 ; check ctx TInt e2 ; TBool
  | If (e1, e2, e3) ->
      check ctx TBool e1 ;
      let ty = type_of ctx e2 in
	check ctx ty e3 ; ty
  | LetRec (f, x, ty1, ty2, e1, e2) ->
      check ((f, TArrow(ty1,ty2)) :: (x, ty1) :: ctx) ty2 e1;
      type_of ((f, TArrow(ty1,ty2)) ::ctx) e2
  | Fun (x, ty1, e) ->
    let ty2 = type_of ((x, ty1) :: ctx) e in
    TArrow (ty1, ty2)
  | Apply (e1, e2) ->
      (match type_of ctx e1 with
	   TArrow (ty1, ty2) -> check ctx ty1 e2 ; ty2
	 | ty ->
	     type_error (string_of_expr e1 ^
			 " is used as a function but its type is "
			 ^ string_of_type ty))
