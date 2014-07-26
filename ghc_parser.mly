
%token <int> INTEGER
%token <int> REGISTER
%token PC
%token COMMA
%token LBRK RBRK
%token MOV INC DEC ADD SUB MUL DIV AND OR XOR JLT JEQ JGT INT HLT
%token EOF
%start <Ghc_instr.instr list> main

%%

main:
  code = list(instr) EOF {code}

instr:
| MOV dest = argument COMMA src = argument {Ghc_instr.MOV (dest, src)}
| INC dest = argument {Ghc_instr.INC dest}
| DEC dest = argument {Ghc_instr.DEC dest}
| ADD dest = argument COMMA src = argument {Ghc_instr.(BINOP (ADD,dest, src))}
| SUB dest = argument COMMA src = argument {Ghc_instr.(BINOP (SUB,dest, src))}
| MUL dest = argument COMMA src = argument {Ghc_instr.(BINOP (MUL,dest, src))}
| DIV dest = argument COMMA src = argument {Ghc_instr.(BINOP (DIV,dest, src))}
| AND dest = argument COMMA src = argument {Ghc_instr.(BINOP (AND,dest, src))}
| OR  dest = argument COMMA src = argument {Ghc_instr.(BINOP (OR,dest, src))}
| XOR dest = argument COMMA src = argument {Ghc_instr.(BINOP (XOR,dest, src))}
| JLT target = INTEGER COMMA x = argument COMMA y = argument
    {Ghc_instr.(JUMP (LT, target, x, y ))}
| JEQ target = INTEGER COMMA x = argument COMMA y = argument
    {Ghc_instr.(JUMP (EQ, target, x, y ))}
| JGT target = INTEGER COMMA x = argument COMMA y = argument
    {Ghc_instr.(JUMP (GT, target, x, y ))}
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
