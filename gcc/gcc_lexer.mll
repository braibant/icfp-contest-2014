{
  open Ghc_parser
  exception Eof
}

rule token = parse
| ';' { comment lexbuf}
| ['\n'] {token lexbuf}
| [' ' '\t' ] {token lexbuf}
| ',' {COMMA}
| "LDC" {LDC}
| "LD" {LD}
| "ADD" {ADD}
| "SUB" {SUB}
| "MUL" {MUL}
| "DIV" {DIV}
| "CEQ" {CEQ}
| "CGT" {CGT}
| "CGTE" {CGTE}
| "ATOM" {ATOM}
| "CONS" {CONS}
| "CAR" {CAR}
| "CDR" {CDR}
| "SEL" {SEL}
| "JOIN" {JOIN}
| "LDF" {LDF}
| "AP" {AP}
| "RTN" {RTN}
| "DUM" {DUM}
| "RAP" {RAP}
| "STOP" {STOP}
| "TSEL" {TSEL}
| "TAP" {TAP}
| "TRAP" {TRAP}
| "ST" {ST}
| "DBUG" {DBUG}
| "BRK" {BRK}
| ['0' - '9']+ as i { INTEGER (int_of_string i)}
| eof {EOF}
and comment = parse
| [^ '\n'] {comment lexbuf}
| ['\n'] {token lexbuf}
| eof {EOF}
