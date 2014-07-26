%token <int> INTEGER
%token COMMA
%token LDC LD ADD SUB MUL DIV CEQ CGT CGTE ATOM CONS CAR CDR SEL JOIN LDF AP RTN DUM RAP STOP TSEL TAP TRAP ST
%token EOF
%start <Gcc_instr.instruction list> main

%%

main:
  code = list(instr) EOF {code}

instr:
| LDC n = INTEGER { Gcc_instr.LDC n }
| LD n = INTEGER COMMA i = INTEGER { Gcc_instr.LD (n, i) }
| ADD { Gcc_instr.ADD }
| SUB { Gcc_instr.SUB }
| MUL { Gcc_instr.MUL }
| DIV { Gcc_instr.DIV }
| CEQ { Gcc_instr.CEQ }
| CGT { Gcc_instr.CGT }
| CGTE { Gcc_instr.CGTE }
| ATOM { Gcc_instr.ATOM }
| CONS { Gcc_instr.CONS }
| CAR { Gcc_instr.CAR }
| CDR { Gcc_instr.CDR }
| SEL n = INTEGER COMMA i = INTEGER { Gcc_instr.SEL (n, i) }
| JOIN { Gcc_instr.JOIN }
| LDF n = INTEGER { Gcc_instr.SEL n }
| AP n = INTEGER { Gcc_instr.AP n }
| RTN { Gcc_instr.RTN }
| DUM n = INTEGER { Gcc_instr.DUM n }
| RAP n = INTEGER { Gcc_instr.RAP n }
| STOP { Gcc_instr.STOP }
| TSEL t = INTEGER f = INTEGER { Gcc_instr.TSEL (t, f) }
| TAP n = INTEGER { Gcc_instr.TAP n }
| TRAP n = INTEGER { Gcc_instr.TRAP n }
| ST n = INTEGER COMMA i = INTEGER { Gcc_instr.ST (n, i) }
| DBUG { Gcc_instr.DBUG }
| BRK { Gcc_instr.BRK }
