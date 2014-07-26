open Printf;;

let error msg =
  eprintf "%s\n" msg;
  exit 2;
;;

module Smap = Map.Make (String);;

let number_labels m ast =
  let f (i, m) line =
    match line with
    | ([], _) -> (i+1, m)
    | (l, _) ->
        let g m lbl =
          if Smap.mem lbl m then error ("label multiply defined: " ^ lbl);
          Smap.add lbl i m
        in
        (i+1, List.fold_left g m l)
  in
  let (_, m) = List.fold_left f (0, Smap.empty) ast in
  m
;;

let get_lbl m s = Smap.find s m;;

open Instr;;

let arg m a =
  match a with
  | Register i -> sprintf "%c" (Char.chr (Char.code 'A' + i))
  | PC -> "PC"
  | Indirect i -> sprintf "[%c]" (Char.chr (Char.code 'A' + i))
  | Constant i -> sprintf "%d" i
  | Location i -> sprintf "[%d]" i
  | Label l -> sprintf "%d" (get_lbl m l)
;;

let binop op =
  match op with
  | ADD -> "ADD"
  | SUB -> "SUB"
  | MUL -> "MUL"
  | DIV -> "DIV"
  | AND -> "AND"
  | OR -> "OR"
  | XOR -> "XOR"
;;

let comp cmp =
  match cmp with
  | LT -> "LT"
  | EQ -> "EQ"
  | GT -> "GT"
;;

let print_instr m i =
  match i with
  | MOV (dst, src) -> printf "MOV %s, %s\n" (arg m dst) (arg m src)
  | INC a -> printf "INC %s\n" (arg m a)
  | DEC a -> printf "DEC %s\n" (arg m a)
  | BINOP (op, dst, src) ->
      printf "%s %s, %s\n" (binop op) (arg m dst) (arg m src)
  | JUMP (cmp, lbl, arg1, arg2) ->
      printf "J%s %d, %s, %s\n" (comp cmp) (get_lbl m lbl) (arg m arg1)
             (arg m arg2)
  | INT i -> printf "INT %d\n" i
  | HLT -> printf "HLT\n"
;;

let print_ast m ast = List.iter (fun (_, i) -> print_instr m i) ast;;

let main () =
  try
    if Array.length Sys.argv < 2 then error "missing input file";
    let c = open_in_bin Sys.argv.(1) in
    let len = in_channel_length c in
    let buf = String.create len in
    really_input c buf 0 len;
    let buf = String.uppercase buf in
    let lexbuf = Lexing.from_string buf in
    let ast = Parser.main Lexer.token lexbuf in
    let m = number_labels Smap.empty ast in
    print_ast m ast;
  with
  | Sys_error msg -> error msg
  | Parser.Error -> error (sprintf "syntax error at line %d" !Lexer.line)
;;

main ();;