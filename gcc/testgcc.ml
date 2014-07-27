#mod_use "../_build/gcc/pp.ml";;
#mod_use "../_build/gcc/gcc_instr.ml";;
#mod_use "../_build/gcc/gcc.ml";;
#mod_use "../_build/gcc/gcc_parser.ml";;
#mod_use "../_build/gcc/gcc_lexer.ml";;

open Gcc;;
open Gcc_instr;;

let gcc_env = tag Int 0;;

let input = {
  s = [];
  e = [Unique [|
    gcc_env;   (* first argument: the state of the world *)
    tag Int 0; (* second argument: unused so far *)
  |] ];
  c = Code 0;
  d = [ control_tag Stop () ];
}

let program = Gcc_lexer.program "../dummy.gcc" |> Array.of_list;;

run program input |> untyped;;
