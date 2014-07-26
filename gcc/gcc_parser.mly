%{

%}

%token <(Lexing.position*Lexing.position)*int> INTEGER
%token <Lexing.position*Lexing.position> COMMA
%token <Lexing.position*Lexing.position> LDC LD ADD SUB MUL DIV CEQ CGT CGTE
					     ATOM CONS CAR CDR SEL JOIN LDF
					     AP RTN DUM RAP STOP TSEL TAP TRAP
					     ST BRK DBUG
%token EOF
%type <Gcc_instr.instruction list> main
%start main
%%

main:
  code = list(instr) EOF {code}

instr:
| LDC n = INTEGER { Gcc_instr.LDC (snd n) }
| LDC error { Format.eprintf "%a@[Error:@ Syntax error after LDC@]@."
			     Gcc_instr.print_position $1;
	      failwith "zut" }
| LD n = INTEGER i = INTEGER { Gcc_instr.LD (snd n, snd i) }
| LD error { Format.eprintf "%a@[Error:@ Syntax error after LD@]@."
			     Gcc_instr.print_position $1;
	      failwith "zut" }
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
| SEL n = INTEGER i = INTEGER
			      { Gcc_instr.SEL
				  (Gcc_instr.Code (snd n),
				   Gcc_instr.Code (snd i)) }
| SEL error { Format.eprintf "%a@[Error:@ Syntax error after SEL@]@."
			     Gcc_instr.print_position $1;
	      failwith "zut" }
| JOIN { Gcc_instr.JOIN }
| LDF n = INTEGER { Gcc_instr.LDF (Gcc_instr.Code (snd n)) }
| LDF error { Format.eprintf "%a@[Error:@ Syntax error after LDF@]@."
			     Gcc_instr.print_position $1;
	      failwith "zut" }
| AP n = INTEGER { Gcc_instr.AP (snd n) }
| AP error { Format.eprintf "%a@[Error:@ Syntax error after AP@]@."
			     Gcc_instr.print_position $1;
	      failwith "zut" }
| RTN { Gcc_instr.RTN }
| DUM n = INTEGER { Gcc_instr.DUM (snd n) }
| DUM error { Format.eprintf "%a@[Error:@ Syntax error after DUM@]@."
			     Gcc_instr.print_position $1;
	      failwith "zut" }
| RAP n = INTEGER { Gcc_instr.RAP (snd n) }
| RAP error { Format.eprintf "%a@[Error:@ Syntax error after RAP@]@."
			     Gcc_instr.print_position $1;
	      failwith "zut" }
| STOP { Gcc_instr.STOP }
| TSEL t = INTEGER f = INTEGER
			 { Gcc_instr.TSEL
			     (Gcc_instr.Code (snd t), Gcc_instr.Code (snd f)) }
| TSEL error { Format.eprintf "%a@[Error:@ Syntax error after TSEL@]@."
			     Gcc_instr.print_position $1;
	      failwith "zut" }
| TAP n = INTEGER { Gcc_instr.TAP (snd n) }
| TAP error { Format.eprintf "%a@[Error:@ Syntax error after TAP@]@."
			     Gcc_instr.print_position $1;
	      failwith "zut" }
| TRAP n = INTEGER { Gcc_instr.TRAP (snd n) }
| TRAP error { Format.eprintf "%a@[Error:@ Syntax error after TRAP@]@."
			      Gcc_instr.print_position $1;
	       failwith "zut" }
| ST n = INTEGER i = INTEGER { Gcc_instr.ST (snd n, snd i) }
| ST error { Format.eprintf "%a@[Error:@ Syntax error after ST@]@."
			     Gcc_instr.print_position $1;
	      failwith "zut" }
| DBUG { Gcc_instr.DBUG }
| BRK { Gcc_instr.BRK }
| error { failwith "Syntax error" }
