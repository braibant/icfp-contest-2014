open Asttypes
open Typedtree
open Parsetree

let include_module str =
  let file =  (String.lowercase str ^ ".ml") in
  let open Misc in
  let open Config in
  Pparse.file Format.std_formatter file Parse.implementation ast_impl_magic_number

let get_include s = match s.pstr_desc with
  | Pstr_open (Asttypes.Override, {Asttypes.txt = Longident.Lident str; _}) ->
    Some str
  | _ -> None

let open_drop = object
  inherit Ast_mapper.mapper as super

  method structure_item s =
    match get_include s with
      | Some _ -> []
      | None -> super#structure_item s
end

let open_include = object
  inherit Ast_mapper.mapper as super

  method structure_item s =
    match get_include s with
      | Some str -> open_drop#structure (include_module str)
      | None -> super#structure_item s
end
