let lines file =
  let i = open_in file in
  let l = ref [] in
  try
    while true do
      l := input_line i :: !l
    done;
    assert false
  with  End_of_file ->
    close_in i;
    List.rev !l

let of_file file =
  let lines = lines file in
  let lines = List.map String.uppercase lines in
  let content = String.concat "\n" lines in
  let lexbuf = Lexing.from_string content in
  let result = Ghc_parser.main Ghc_lexer.token lexbuf in
  Array.of_list result


open Ghc_instr

let pp_binop fmt =
  let open Format in
  function
  | ADD -> fprintf fmt "ADD"
  | SUB -> fprintf fmt "SUB"
  | MUL -> fprintf fmt "MUL"
  | DIV -> fprintf fmt "DIV"
  | AND -> fprintf fmt "AND"
  | OR  -> fprintf fmt "OR "
  | XOR -> fprintf fmt "XOR"

let pp_comp fmt =
  let open Format in
  function
  | LT -> fprintf fmt "LT"
  | EQ -> fprintf fmt "EQ"
  | GT -> fprintf fmt "GT"

let pp_arg fmt =
  let open Format in
  function
  | Register i -> fprintf fmt "%c" (char_of_int (int_of_char 'A' + i))
  | PC         -> fprintf fmt "PC"
  | Indirect i -> fprintf fmt "[%c]" (char_of_int (int_of_char 'A' + i))
  | Constant i -> fprintf fmt "%i" i
  | Location i -> fprintf fmt "[%i]" i

let pp_instr fmt =
  let open Format in
  function
  | MOV (dest, src) -> fprintf fmt "MOV %a,%a" pp_arg dest pp_arg src

  (* Math *)
  | INC dest -> fprintf fmt "INC %a" pp_arg dest
  | DEC dest -> fprintf fmt "DEC %a" pp_arg dest
  | BINOP (op, dest, src) ->
     fprintf fmt "%a %a,%a" pp_binop op pp_arg dest pp_arg src
  | JUMP (comp, target, x, y) ->
     fprintf fmt "J%a %i,%a,%a" pp_comp comp target pp_arg x pp_arg y
  | INT i ->
     fprintf fmt "INT %i" i
  | HLT ->
     fprintf fmt "HLT"

let pp fmt code =
  Array.iter (fun instr -> Format.fprintf fmt "%a\n" pp_instr instr) code
