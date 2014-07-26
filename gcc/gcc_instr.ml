type code_ptr = Code of int

type instruction =
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

let print_instruction fmt = function
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


let print_instructions fmt l =
  Pp.print_list Pp.newline print_instruction fmt l
