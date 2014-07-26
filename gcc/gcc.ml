(* warnings stolen from Mezzo:
   "@1..3@8..12@14..21@23..40-41@43" *)

open Gcc_instr

module Counted : sig
  type 'a t = private (int * 'a)
  val fresh : 'a -> 'a t
end = struct
  type 'a t = int * 'a
  let count = ref 0
  let fresh v =
    let i = !count in
    incr count;
    (i, v)
end

type registers = {
  s : data_stack;
  e : environment;
  c : code_ptr;
  d : control_stack;
}
and data_stack = value list
and control_stack = control list
and environment = frame list
and frame =
| Unique of value array
| Shared of shared_frame
and shared_frame = shared_frame_state ref Counted.t
and shared_frame_state =
| Dummy of int
| Defined of value array

and 'a stack_tag =
| S : value stack_tag
| E : frame stack_tag
| D : control stack_tag

and value = Value : 'a value_tag * 'a -> value
and _ value_tag =
| Int : int value_tag
| Pair : (value * value) value_tag
| Closure : (code_ptr * environment) value_tag

and control = Control : 'a control_tag * 'a -> control
and _ control_tag =
| Join : code_ptr control_tag
| Ret : (code_ptr * environment) control_tag
| Stop : unit control_tag

type code = instruction array

type ('a, 'b) tag_error = { expected: 'a; found : 'b; }

type step_error =
    | Empty_stack : 'a stack_tag -> step_error
    | Invalid_frame_index
    | Dummy_frame
    | Invalid_code_pointer
    | Value_tag_mismatch
        : ('a value_tag, 'b value_tag) tag_error -> step_error
    | Control_tag_mismatch
        : ('a control_tag, 'b control_tag) tag_error -> step_error
    | Division_by_zero

let rec print_value fmt = function
  | Value(Int,i) -> Format.fprintf fmt "%i" i
  | Value(Pair,(v1,v2)) -> Format.fprintf fmt "(%a,%a)"
                           print_value v1 print_value v2
  | Value(Closure,(Code c,_)) -> Format.fprintf fmt "(closure %i,_)" c

exception Machine_stop
exception Break_reached
exception Step_error of step_error

let error err = raise (Step_error err)

let tag : type a . a value_tag -> a -> value =
  fun ty v -> match ty with
    | Int -> Value (Int, v)
    | Pair -> Value (Pair, v)
    | Closure -> Value (Closure, v)

let untag : type a . a value_tag -> value -> a =
  fun ty v -> match ty, v with
    | Int, Value (Int, n) -> n
    | Pair, Value (Pair, p) -> p
    | Closure, Value (Closure, ptr) -> ptr
    | expected, Value (found, _) ->
      error (Value_tag_mismatch { expected; found; })

let control_tag ty v = Control (ty, v)

let control_untag : type a . a control_tag -> control -> a =
 fun ty v -> match ty, v with
   | Join, Control (Join, v) -> v
   | Ret, Control (Ret, v) -> v
   | Stop, Control (Stop, v) -> v
   | expected, Control (found, _) ->
     error (Control_tag_mismatch { expected; found; })

let next_instr (Code n) = Code (n + 1)

let pop : type a . a stack_tag -> a list -> a * a list =
 fun tag -> function
  | [] -> error (Empty_stack tag)
  | x::stack -> (x, stack)

let nth : type a . a stack_tag -> a list -> int -> a =
 fun tag stack n ->
   try List.nth stack n
   with _ -> error (Empty_stack tag)

let get_shared_frame counted_shared_frame =
  let (_, frame) = (counted_shared_frame : shared_frame :> _ * _) in
  match !frame with
    | Dummy _ -> error Dummy_frame
    | Defined frame -> frame

let set_shared_frame counted_shared_frame args =
  let (_, frame) = (counted_shared_frame : shared_frame :> _ * _) in
  match !frame with
    | Defined _ -> error Dummy_frame
    | Dummy n ->
      if Array.length args <> n
      then error Dummy_frame
      else frame := Defined args

let frame_index_check frame i =
  if Array.length frame <= i
  then error Invalid_frame_index

let frame_get frame i =
  let tab = match frame with
    | Unique tab -> tab
    | Shared frame -> get_shared_frame frame in
  frame_index_check tab i;
  tab.(i)

let frame_set frame i v =
  let tab = match frame with
    | Unique tab -> tab
    | Shared frame -> get_shared_frame frame in
  frame_index_check tab i;
  tab.(i) <- v

let ldc n {s;e;c;d} =
  let s = tag Int n :: s in
  let c = next_instr c in
  {s;e;c;d}

let ld n i {s;e;c;d} =
  let frame = nth E e n in
  let v = frame_get frame i in
  let s = v::s in
  let c = next_instr c in
  {s;e;c;d}

exception Data_stack_overflow

let unop op {s;e;c;d} =
  let x, s = pop S s in
  let s = op x :: s in
  let c = next_instr c in
  {s;e;c;d}

let binop op {s;e;c;d} =
  let y, s = pop S s in
  let x, s = pop S s in
  let s = op x y :: s in
  let c = next_instr c in
  {s;e;c;d}

let intop op x y = tag Int (op (untag Int x) (untag Int y))

let add = binop (intop (+))
let sub = binop (intop (-))
let mul = binop (intop ( * ))
let div =
  let div a b =
    if b = 0 then error Division_by_zero
    else a / b in
  binop (intop div)

let cmpop cmp = intop (fun x y -> if cmp x y then 1 else 0)
let ceq = binop (cmpop (=))
let cgt = binop (cmpop (>))
let cgte = binop (cmpop (>=))

let atom =
  unop (fun (Value (ty, _)) ->
    match ty with
      | Int -> tag Int 1
      | _ -> tag Int 0
  )

let cons = binop (fun x y -> tag Pair (x, y))

let car = unop (fun v ->
  let (x, _y) = untag Pair v in
  x
)

let cdr = unop (fun v ->
  let (_x, y) = untag Pair v in
  y
)

type tail_status =
| Tail
| Non_tail

let sel tail t f {s;e;c;d} =
  let x, s = pop S s in
  let n = untag Int x in
  let d = match tail with
    | Tail -> d
    | Non_tail -> control_tag Join (next_instr c) :: d in
  let c = if n = 0 then f else t in
  {s;e;c;d}

let join {s;e;c=_;d} =
  let x, d = pop D d in
  let c = control_untag Join x in
  {s;e;c;d}

let ldf f {s;e;c;d} =
  let s = tag Closure (f,e) :: s in
  let c = next_instr c in
  {s;e;c;d}

let eat_args n s =
  let cur_s = ref s in
  let args = ref [] in
  for _i = 0 to n - 1 do
    let x, s = pop S !cur_s in
    args := x :: !args;
    cur_s := s;
  done;
  Array.of_list !args, !cur_s

let ap tail n {s;e;c;d} =
  let x, s = pop S s in
  let (closure_code, closed_env) = untag Closure x in
  let frame, s = eat_args n s in
  let d = match tail with
    | Tail -> d
    | Non_tail ->
      (* a difference from the spec:
         Ret control slots keep both
         the code pointer and the environment,
         instead of storing them as two separate stack slots *)
      control_tag Ret (next_instr c, e) :: d in
  let e = Unique frame :: closed_env in
  let c = closure_code in
  {s;e;c;d}

let rtn {s;e=_;c=_;d} =
  let x, d = pop D d in
  begin
    let Control (tag, _) = x in
    match tag with
      | Stop -> raise Machine_stop
      | _ -> ()
  end;
  (* difference from the spec: see [ap] above *)
  let c, e = control_untag Ret x in
  {s;e;c;d}

let dum n {s;e;c;d} =
  let frame = Counted.fresh (ref (Dummy n)) in
  let e = Shared frame :: e in
  let c = next_instr c in
  {s;e;c;d}

let rap tail n {s;e;c;d} =
  let x,s = pop S s in
  let (f, fp) = untag Closure x in
  let xframe, _ = pop E fp in
  let eframe, ep = pop E e in
  let shared_frame = match xframe with
    | Unique _ -> error Dummy_frame
    | Shared frame -> frame
  in
  if not (xframe == eframe) then error Dummy_frame;
  let frame, s = eat_args n s in
  set_shared_frame shared_frame frame;
  let d = match tail with
    | Tail -> d
    | Non_tail -> control_tag Ret (next_instr c, ep) :: d in
  let e = fp in
  let c = f in
  {s;e;c;d}

let stop _reg = raise Machine_stop

let st n i {s;e;c;d} =
  let frame = nth E e n in
  let v, s = pop S s in
  frame_set frame i v;
  let c = next_instr c in
  {s;e;c;d}

let step inst = match inst with
| LDC n -> ldc n
| LD (n, i) -> ld n i
| ADD -> add
| SUB -> sub
| MUL -> mul
| DIV -> div
| CEQ -> ceq
| CGT -> cgt
| CGTE -> cgte
| ATOM -> atom
| CONS -> cons
| CAR -> car
| CDR -> cdr
| SEL (t, f) -> sel Non_tail t f
| JOIN -> join
| LDF f -> ldf f
| AP n -> ap Non_tail n
| RTN -> rtn
| DUM n -> dum n
| RAP n -> rap Non_tail n
| STOP -> stop
| TSEL (t, f) -> sel Tail t f
| TAP n -> ap Tail n
| TRAP n -> rap Tail n
| ST (n, i) -> st n i
| BRK -> raise Break_reached
(* currently unsupported *)
| DBUG -> failwith "instruction DBUG not yet implemented"

let init_regs =  {
    s = [];
    e = [];
    c = Code 0;
    d = [];
}

type run_error =
| Stack_overflow : 'a stack_tag -> run_error
| Step of step_error * instruction
| Invalid_code_pointer

exception Run_error of (run_error * registers)

let print_stack_tag (type a) fmt: a stack_tag -> unit = function
| S -> Format.fprintf fmt "value"
| E -> Format.fprintf fmt "frame"
| D -> Format.fprintf fmt "control"

let print_value_tag (type a) fmt: a value_tag -> unit = function
| Int -> Format.fprintf fmt "Int"
| Pair -> Format.fprintf fmt "Pair"
| Closure -> Format.fprintf fmt "Closure"

let print_step_error fmt = function
  | Empty_stack s -> Format.fprintf fmt "Empty stack %a" print_stack_tag s
  | Invalid_frame_index -> Format.fprintf fmt "Invalid frame index"
  | Dummy_frame -> Format.fprintf fmt "Dummy frame"
  | Invalid_code_pointer -> Format.fprintf fmt "Invalid code pointer"
  | Value_tag_mismatch v ->
    Format.fprintf fmt "Value_tag_mismatch(exp=%a,found=%a)"
      print_value_tag v.expected print_value_tag v.found
  | Control_tag_mismatch _ -> Format.fprintf fmt "Control tag mismatch"
  | Division_by_zero -> Format.fprintf fmt "Division by zero"


let print_run_error fmt = function
  | Stack_overflow v -> Format.fprintf fmt "Stack overflow %a"
                          print_stack_tag v
  | Step (s,_) -> Format.fprintf fmt "Step Error:%a" print_step_error s
  | Invalid_code_pointer -> Format.fprintf fmt "Invalid code pointer"

let stack_limit = 10_000

let check_stacks ({s;e;d;_} as regs) =
  let overflow stack_tag =
    raise (Run_error (Stack_overflow stack_tag, regs)) in
  if List.length s > stack_limit then overflow S;
  if List.length e > stack_limit then overflow E;
  if List.length d > stack_limit then overflow D;
  ()

let rec run code regs =
  let error err = raise (Run_error (err, regs)) in
  check_stacks regs;
  let instr =
    let Code ptr = regs.c in
    try code.(ptr) with _ ->
      error Invalid_code_pointer in
  match begin
    try `Step (step instr regs) with
      | Step_error err -> `Error (Step (err, instr))
      | Machine_stop -> `Stop
  end with
    | `Step regs -> run code regs
    | `Error err -> error err
    | `Stop -> regs

(** Examples *)
let local =
  let body = Code 4 in
  [|
  (* 0 *) LDC 21;
  (* 1 *) LDF body; (* load body *)
  (* 2 *) AP 1;     (* call body with 1 variable in a new frame *)
  (* 3 *) STOP;
  (* note: online example uses RTN instead of STOP here,
     which gives a control stack error *)
  (* body = 4 *)
  (* 4 *) LD (0, 0); (* var x *)
  (* 5 *) LD (0, 0); (* var x *)
  (* 6 *) ADD;
  (* 7 *) RTN;
  |]

let () = begin
  match run local init_regs with
    | {
        s = [Value (Int, 42)];
        e = [];
        c = Code 3;
        d = [];
      } -> ()
    | _ -> failwith "local.gcc test failed"
end

let goto =
  let main = Code 6 in
  let to_ = Code 10 in
  let go = Code 16 in
  [|
(* 0 *)  DUM  2        ; (* 2 top-level declarations *)
(* 1 *)  LDF  go       ; (* declare function go *)
(* 2 *)  LDF  to_      ; (* declare function to *)
(* 3 *)  LDF  main     ; (* main function *)
(* 4 *)  RAP  2        ; (* load declarations into environment and run main *)
(* 5 *)  RTN           ; (* final return *)
(* main: *)
(* 6 *)  LDC  1        ;
(* 7 *)  LD   (0, 0)   ; (* var go *)
(* 8 *)  AP   1        ; (* call go(1) *)
(* 9 *)  RTN           ;
(* to: *)
(* 10 *)  LD   (0, 0)  ; (* var n *)
(* 11 *)  LDC  1       ;
(* 12 *)  SUB          ;
(* 13 *)  LD   (1, 0)   ; (* var go *)
(* 14 *)  AP   1        ; (* call go(n-1) *)
(* 15 *)  RTN           ;
(* go: *)
(* 16 *)  LD   (0, 0)   ; (* var n *)
(* 17 *)  LDC  1        ;
(* 18 *)  ADD           ;
(* 19 *)  LD   (1, 1)   ; (* var to *)
(* 20 *)  AP   1        ; (* call to(n+1) *)
(* 21 *)  RTN           ;
    |]

(** Useful for debugging: toplevel doesn't pretty-printer GADTs as one
    would expect *)
module Untyped = struct
  type code_ptr = int

  type registers = {
    s : data_stack;
    e : environment;
    c : code_ptr;
    d : control_stack;
  }
  and data_stack = value list
  and control_stack = control list
  and environment = frame list
  and frame =
  | Unique of value array
  | Shared_seen of int
  | Shared_new of int * frame_state
  and frame_state =
  | Dummy of int | Defined of value array

  and value =
  | Int of int
  | Pair of value * value
  | Closure of int * environment

  and control =
  | Join of code_ptr
  | Ret of code_ptr * environment
  | Stop
end

let untyped : registers -> Untyped.registers =
  let seen = Hashtbl.create 10 in
  let rec untyped_data_stack s = List.map untyped_value s
  and untyped_control_stack d = List.map untyped_control d
  and untyped_environment e = List.map untyped_frame e
  and untyped_frame frame = match frame with
    | Unique value -> Untyped.Unique (Array.map untyped_value value)
    | Shared shared_frame ->
      let (count, frame_state) = (shared_frame : shared_frame :> _ * _) in
      if Hashtbl.mem seen count then Untyped.Shared_seen count
      else begin
        Hashtbl.add seen count ();
        let state = match !frame_state with
          | Dummy n -> Untyped.Dummy n
          | Defined v -> Untyped.Defined (Array.map untyped_value v)
        in
        Untyped.Shared_new (count, state)
      end
  and untyped_code_ptr (Code n) = n
  and untyped_value : value -> Untyped.value = function
    | Value (Int, n) ->
      Untyped.Int n
    | Value (Pair, (a, b)) ->
      Untyped.Pair (untyped_value a, untyped_value b)
    | Value (Closure, (c, env)) ->
      Untyped.Closure (untyped_code_ptr c, untyped_environment env)
  and untyped_control = function
    | Control (Join, c) ->
      Untyped.Join (untyped_code_ptr c)
    | Control (Ret, (c, e)) ->
      Untyped.Ret (untyped_code_ptr c, untyped_environment e)
    | Control (Stop, ()) ->
      Untyped.Stop
  in
  fun {s;e;c;d} ->
    let s = untyped_data_stack s in
    let e = untyped_environment e in
    let c = untyped_code_ptr c in
    let d = untyped_control_stack d in
    Untyped.({s;e;c;d})
