#mod_use "_build/gcc/pp.ml";;
#mod_use "_build/gcc/gcc_instr.ml";;
#mod_use "_build/gcc/gcc.ml";;
#mod_use "_build/gcc/gcc_parser.ml";;
#mod_use "_build/gcc/gcc_lexer.ml";;

open Gcc;;
open Gcc_instr;;

(* dummy map state *)
let gcc_env = tag Int 0;;

(* initialization input *)
let input = {
  s = [];
  e = [Unique [|
    gcc_env;   (* first argument: the state of the world *)
    tag Int 0; (* second argument: unused so far *)
  |] ];
  c = Code 0;
  d = [ control_tag Stop () ];
}

let lambda_program = Gcc_lexer.program "random.gcc" |> Array.of_list

let _ = run lambda_program input |> untyped

let lambda_state, lambda_step =
  match run lambda_program input with
    | {
      s = [Value (Pair, (state, step))];
      e = [Unique [|_; _|]];
      c = _;
      d = [];
    } -> state, step
    | _ -> failwith "gcc initialization failure"

(* stepping input *)
let input =
  {
    s = [
      lambda_step;   (* function to call: the step function *)
      gcc_env;       (* second argument: the state of the world *)
      lambda_state;  (* first argument: the AI state *)
    ];
    e = [];
    c = Gcc_instr.Code 0;
    d = [control_tag Stop ()];
  }
  |> ap Tail 2 (* call the step function *)

let _ = input |> untyped

let _ = run lambda_program input |> untyped
