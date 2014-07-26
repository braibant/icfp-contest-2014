{
  open Parser

  let newline lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

let gen_report_position fmt (f,l,b,e) =
  Format.fprintf fmt "File \"%s\", line %d, characters %d-%d" f l b e

let report_position fmt = Format.fprintf fmt "%a:@\n" gen_report_position

let extract (b,e) =
  let open Lexing in
  let f = b.pos_fname in
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol in
  let lc = e.pos_cnum - b.pos_bol in
  (f,l,fc,lc)

let loc lb = extract (Lexing.lexeme_start_p lb, Lexing.lexeme_end_p lb)

let set_file file lb =
  lb.Lexing.lex_curr_p <-
    { lb.Lexing.lex_curr_p with Lexing.pos_fname = file }


}

let var = ['a'-'z' 'A'-'Z']+

rule token = parse
  | [' ' '\t']   { token lexbuf }
  | [ '\r' '\n'] { newline lexbuf; token lexbuf }
  | ['0'-'9']+           { INT (int_of_string(Lexing.lexeme lexbuf)) }
  | "int"                { TINT }
  | "bool"               { TBOOL }
  | "true"               { TRUE }
  | "false"               { FALSE }
  | "fun"           { FUN }
  | "rec"           { REC }
  | "is"            { IS }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "let"           { LET }
  | "in"           { IN }
  | ";;"            { SEMICOLON2 }
  | '='             { EQUAL }
  | '<'             { LT }
  | "<="            { LTE }
  | '>'             { GT }
  | ">="            { GTE }
  | "->"            { TARROW }
  | ':'             { COLON }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { TIMES }
  | var             { VAR (Lexing.lexeme lexbuf) }
  | eof             { EOF }

{
}
