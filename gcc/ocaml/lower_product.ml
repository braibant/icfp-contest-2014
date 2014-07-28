(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*    Thomas Gazagnaire (OCamlPro), Fabrice Le Fessant (INRIA Saclay)     *)
(*                                                                        *)
(*   Copyright 2007 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Typedtree
open Parsetree

(*
Some notes:

   * For Pexp_function, we cannot go back to the exact original version
   when there is a default argument, because the default argument is
   translated in the typer. The code, if printed, will not be parsable because
   new generated identifiers are not correct.

   * For Pexp_apply, it is unclear whether arguments are reordered, especially
    when there are optional arguments.

  * TODO: check Ttype_variant -> Ptype_variant (stub None)

*)

let rec lower_exp_tuple loc = function
  | ([] | [_] | [_; _]) as small -> Pexp_tuple small
  | x::xs -> Pexp_tuple [x; {pexp_desc = lower_exp_tuple loc xs; pexp_loc = loc}]

let rec lower_pat_tuple loc = function
  | ([] | [_] | [_; _]) as small -> Ppat_tuple small
  | x::xs -> Ppat_tuple [x; {ppat_desc = lower_pat_tuple loc xs; ppat_loc = loc}]

let rec lower_typ_tuple loc = function
  | ([] | [_] | [_; _]) as small -> Ptyp_tuple small
  | x::xs -> Ptyp_tuple [x; {ptyp_desc = lower_typ_tuple loc xs; ptyp_loc = loc}]


(* TODO: lowering functions that remain to be done *)
let lower_field _loc (exp, lid, _label) = Pexp_field (exp, lid)
let lower_setfield _loc (exp1, lid, _label, exp2) =
  failwith "lower_product.ml: mutable fields are unsupported"
let lower_type_variant _loc vli =
  (* TODO enforce at-most-one-non-constant restriction *)
  Ptype_variant vli
let lower_type_record _loc vli = Ptype_record vli
let lower_pat_record _loc (list, closed) = Ppat_record (list, closed)
let lower_exp_record _loc (list, expo) = Pexp_record (list, expo)

let include_module str =
  let file =  (String.lowercase str ^ ".ml") in
  let open Misc in
  let open Config in
  Pparse.file Format.std_formatter file Parse.implementation ast_impl_magic_number

let lower = object
  inherit Ast_mapper.mapper as super

  method expr e =
    let loc = e.pexp_loc in
    match e.pexp_desc with
          | Pexp_tuple tup ->
            { e with pexp_desc = lower_exp_tuple loc tup }
          | other -> super#expr e

  method pat p =
    let loc = p.ppat_loc in
    match p.ppat_desc with
      | Ppat_tuple tup ->
        { p with ppat_desc = lower_pat_tuple loc tup }
      | other -> super#pat p

  method typ t =
    let loc = t.ptyp_loc in
    match t.ptyp_desc with
      | Ptyp_tuple tup ->
        { t with ptyp_desc = lower_typ_tuple loc tup }
      | other -> super#typ t

  method structure_item s =
    let loc = s.pstr_loc in
    match s.pstr_desc with
      | Pstr_open (Asttypes.Override, {Asttypes.txt = Longident.Lident str; _}) ->
        include_module str
      | other -> super#structure_item s
end
