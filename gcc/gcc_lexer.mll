{
  open Gcc_parser
  exception Eof

  let get_pos lexbuf =
    (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
}

rule token = parse
| ';' { comment lexbuf}
| ['\n'] {Lexing.new_line lexbuf; token lexbuf}
| [' ' '\t' ] {token lexbuf}
| ',' {COMMA (get_pos lexbuf)}
| "LDC" {LDC (get_pos lexbuf)}
| "LD" {LD(get_pos lexbuf)}
| "ADD" {ADD(get_pos lexbuf)}
| "SUB" {SUB(get_pos lexbuf)}
| "MUL" {MUL(get_pos lexbuf)}
| "DIV" {DIV(get_pos lexbuf)}
| "CEQ" {CEQ(get_pos lexbuf)}
| "CGT" {CGT(get_pos lexbuf)}
| "CGTE" {CGTE(get_pos lexbuf)}
| "ATOM" {ATOM(get_pos lexbuf)}
| "CONS" {CONS(get_pos lexbuf)}
| "CAR" {CAR(get_pos lexbuf)}
| "CDR" {CDR(get_pos lexbuf)}
| "SEL" {SEL(get_pos lexbuf)}
| "JOIN" {JOIN(get_pos lexbuf)}
| "LDF" {LDF(get_pos lexbuf)}
| "AP" {AP(get_pos lexbuf)}
| "RTN" {RTN(get_pos lexbuf)}
| "DUM" {DUM(get_pos lexbuf)}
| "RAP" {RAP(get_pos lexbuf)}
| "STOP" {STOP(get_pos lexbuf)}
| "TSEL" {TSEL(get_pos lexbuf)}
| "TAP" {TAP(get_pos lexbuf)}
| "TRAP" {TRAP(get_pos lexbuf)}
| "ST" {ST(get_pos lexbuf)}
| "DBUG" {DBUG(get_pos lexbuf)}
| "BRK" {BRK(get_pos lexbuf)}
| ['0' - '9']+ as i { INTEGER (get_pos lexbuf, int_of_string i)}
| eof {EOF}
| _ { Format.eprintf "%a@[ Error:@ Lexing error@]@."
		     Gcc_instr.print_position (get_pos lexbuf);
      failwith "raté"}
and comment = parse
| [^ '\n'] {comment lexbuf}
| ['\n'] {Lexing.new_line lexbuf; token lexbuf}
| eof {EOF}


{
  let program input =
    let lexbuf = Lexing.from_channel input in
    Gcc_parser.main token lexbuf
}
