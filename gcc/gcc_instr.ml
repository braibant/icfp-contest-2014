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
