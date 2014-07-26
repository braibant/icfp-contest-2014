let lines file =
  let i = open_in file in
  let l = ref [] in
  try
    while true do
      l := input_line i :: !l
    done;
    assert false
  with  End_of_file ->
    close_in i;
    List.rev !l

let of_file file =
  let lines = lines file in
  let lines = List.map String.uppercase lines in
  let content = String.concat "\n" lines in
  let lexbuf = Lexing.from_string content in
  let result = Ghc_parser.main Ghc_lexer.token lexbuf in
  Printf.eprintf "%s %i\n" file (List.length result);
  result
