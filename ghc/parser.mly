
%token <int> INTEGER
%token <int> REGISTER
%token <string> LABEL
%token <string> GLOBAL
%token PC
%token COMMA
%token COLON
%token LBRK RBRK
%token MOV INC DEC ADD SUB MUL DIV AND OR XOR JLT JEQ JGT INT HLT
%token JMP
%token EOF
%start < (string * Instr.argument) list* (string list * Instr.instr) list> main

%%

main:
  globals = list(global)
  code = list(line) EOF {globals, code}

global:
  l = GLOBAL COLON COLON a = argument {l,a}

line:
  l = labels i = instr { (l, i) }

instr:
| MOV dest = argument COMMA src = argument {Instr.MOV (dest, src)}
| INC dest = argument {Instr.INC dest}
| DEC dest = argument {Instr.DEC dest}
| ADD dest = argument COMMA src = argument {Instr.(BINOP (ADD,dest, src))}
| SUB dest = argument COMMA src = argument {Instr.(BINOP (SUB,dest, src))}
| MUL dest = argument COMMA src = argument {Instr.(BINOP (MUL,dest, src))}
| DIV dest = argument COMMA src = argument {Instr.(BINOP (DIV,dest, src))}
| AND dest = argument COMMA src = argument {Instr.(BINOP (AND,dest, src))}
| OR  dest = argument COMMA src = argument {Instr.(BINOP (OR,dest, src))}
| XOR dest = argument COMMA src = argument {Instr.(BINOP (XOR,dest, src))}
| JLT target = LABEL COMMA x = argument COMMA y = argument
    {Instr.(JUMP (LT, target, x, y ))}
| JEQ target = LABEL COMMA x = argument COMMA y = argument
    {Instr.(JUMP (EQ, target, x, y ))}
| JGT target = LABEL COMMA x = argument COMMA y = argument
    {Instr.(JUMP (GT, target, x, y ))}
| JMP target = argument { Instr.(MOV (PC, target)) }
| INT i = INTEGER
    {Instr.INT i}
| HLT
    {Instr.HLT}

argument:
| i = REGISTER {Instr.Register i}
| PC { Instr.PC}
| LBRK i = REGISTER RBRK {Instr.Indirect i}
| i = INTEGER {Instr.Constant i}
| LBRK i = INTEGER RBRK {Instr.Location i}
| l = LABEL {Instr.Label l}
| g = GLOBAL {Instr.Global g}

labels:
| l = LABEL COLON tl = labels { l :: tl }
| { [] }
