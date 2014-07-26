
%token <int> INTEGER
%token <int> REGISTER
%token PC
%token COMMA
%token LBRK RBRK
%token MOV INC DEC ADD SUB MUL DIV AND OR XOR JLT JEQ JGT INT HLT

%start <Ghc_types.instr list> main

main:
  code = list(instr) {code}

instr:
| MOV dest = argument COMMA src = argument {Ghc_instr.MOV (dest, src)}
| INC dest = argument {Ghc_instr.INC dest}
| INC dest = argument {Ghc_instr.INC dest}
| ADD dest = argument COMMA src = argument {Ghc_instr.ADD (dest, src)}
| SUB dest = argument COMMA src = argument {Ghc_instr.SUB (dest, src)}
| MUL dest = argument COMMA src = argument {Ghc_instr.MUL (dest, src)}
| DIV dest = argument COMMA src = argument {Ghc_instr.DIV (dest, src)}
| AND dest = argument COMMA src = argument {Ghc_instr.AND (dest, src)}
| OR  dest = argument COMMA src = argument {Ghc_instr.OR (dest, src)}
| XOR dest = argument COMMA src = argument {Ghc_instr.XOR (dest, src)}
| JLT target = INTEGER COMMA x = argument COMMA y = argument
    {Ghc_instr.JUMP (LT, target, x, y )}
| JEQ target = INTEGER COMMA x = argument COMMA y = argument
    {Ghc_instr.JUMP (EQ, target, x, y )}
| JGT target = INTEGER COMMA x = argument COMMA y = argument
    {Ghc_instr.JUMP (GT, target, x, y )}
| INT i = INTEGER
    {Ghc_instr.INT i}
| HLT
    {Ghc_instr.HLT}

argument:
| i = REGISTER {Ghc_instr.Register i}
| PC { Ghc_instr.PC}
| LBRK i = REGISTER RBRK {Ghc_instr.Indirect i}
| i = INTEGER {Ghc_instr.Constant i}
| LBRK i = INTEGER RBRK {Ghc_instr.Location i}
