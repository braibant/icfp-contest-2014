{
  open Parser
  let line = ref 1
}

rule token = parse
| ';' { comment lexbuf}
| ['\n'] {incr line; token lexbuf}
| [' ' '\t' ] {token lexbuf}
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

| "JMP" {JMP}

| [ 'A' - 'H' ] as reg { REGISTER (int_of_char reg - int_of_char 'A')}
| "PC" {PC}
| ['0' - '9']+ as i { INTEGER (int_of_string i)}
| ':' {COLON}
| [ 'A' - 'Z' '0' - '9' '_' ]+ as label { LABEL label }
| eof {line := 1; EOF}
| _ {
  let msg =
     Printf.sprintf "bad character %S at line %d" (Lexing.lexeme lexbuf) !line
  in
  failwith msg
}
and comment = parse
| [^ '\n'] {comment lexbuf}
| ['\n'] {incr line; token lexbuf}
| eof {line := 1; EOF}
