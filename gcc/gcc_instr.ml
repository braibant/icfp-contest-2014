type code_ptr = Code of int

type instruction =
  | COMMENT of string
| LDC of int
| LD of int * int
| ADD
| SUB
| MUL
| DIV
| CEQ
| CGT
| CGTE
| ATOM
| CONS
| CAR
| CDR
| SEL of code_ptr * code_ptr
| JOIN
| LDF of code_ptr
| AP of int
| RTN
| DUM of int
| RAP of int
| STOP
| TSEL of code_ptr * code_ptr
| TAP of int
| TRAP of int
| ST of int * int

(* currently unsupported by the simulator*)
| DBUG
| BRK

type code = instruction array

open Format

let print_comment = ref false

let print_instruction fmt instr =
  match instr with
  | COMMENT s -> if !print_comment then fprintf fmt ";Lambda: %s" s
  | LDC i -> fprintf fmt "LDC %i" i
  | LD(i,j) -> fprintf fmt "LD %i %i" i j
  | ADD -> fprintf fmt "ADD"
  | SUB -> fprintf fmt "SUB"
  | MUL -> fprintf fmt "MUL"
  | DIV -> fprintf fmt "DIV"
  | CEQ -> fprintf fmt "CEQ"
  | CGT -> fprintf fmt "CGT"
  | CGTE -> fprintf fmt "CGTE"
  | ATOM -> fprintf fmt "ATOM"
  | CONS -> fprintf fmt "CONS"
  | CAR -> fprintf fmt "CAR"
  | CDR -> fprintf fmt "CDR"
  | SEL (Code i,Code j) -> fprintf fmt "SEL %i %i" i j
  | JOIN -> fprintf fmt "JOIN"
  | LDF (Code p) -> fprintf fmt "LDF %i" p
  | AP i -> fprintf fmt "AP %i" i
  | RTN -> fprintf fmt "RTN"
  | DUM i -> fprintf fmt "DUM %i" i
  | RAP i -> fprintf fmt "RAP %i" i
  | STOP -> fprintf fmt "STOP"
  | TSEL(Code i,Code j) -> fprintf fmt "TSEL %i %i" i j
  | TAP i -> fprintf fmt "TAP %i" i
  | TRAP i -> fprintf fmt "TRAP %i" i
  | ST(i,j) -> fprintf fmt "ST %i %i" i j
  | DBUG -> fprintf fmt "DBUG@."
  | BRK -> fprintf fmt "BRK@."


let print_instruction r fmt instr =
  print_instruction fmt instr;
  (* for some reason the online interpreter
     doesn't tolerate comments after RTN *)
  begin match instr with
    |RTN -> incr r
    | COMMENT _ -> ()
    | _ -> fprintf fmt "   ; %3i " !r; incr r
  end


let print_instructions fmt l =
  Pp.print_list Pp.newline (print_instruction (ref 0)) fmt l

let print_position fmt (pos,pos_end) =
  Format.fprintf fmt "File \"%s\", line %i, characters %i-%i:@."
		 pos.Lexing.pos_fname pos.Lexing.pos_lnum
		 (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
		 (pos_end.Lexing.pos_cnum - pos.Lexing.pos_bol)
