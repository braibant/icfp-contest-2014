{
  open Ghc_parser
  exception Eof
}

rule token = parse
| ';' { comment lexbuf}
| '\n' {EOL}
| [' ' '\t'] {token lexbuf}
| ',' {COMMA}
| '[' {LBRK}
| ']' {RBRK}
| "MOV" {MOV}
| "INC" {INC}
| "DEC" {DEC}
| "ADD" {ADD}
| "SUB" {SUB}
| "MUL" {MUL}
| "DIV" {DIV}
| "AND" {AND}
| "OR"  {OR}
| "XOR" {XOR}
| "JLT" {JLT}
| "JEQ" {JEQ}
| "JGT" {JGT}
| "INT" {INT}
| "HLT" {HLT}
| [ 'A' - 'H' ] as reg { REGISTER (int_of_char reg - int_of_char 'A')}
| ['0' - '9']+ as i { INTEGER (string_of_int i)}
| eof {raise Eof}
and comment = parse | [^ '\n'] {comment lexbuf}
