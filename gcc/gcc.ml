type dummy = Dummy

type registers = {
  s : data_stack;
  e : environment;
  c : code_ptr;
  d : control_stack;
}
and data_stack = value list
and control_stack = control list
and environment = frame list
and frame = value array

and 'a stack_tag =
| S : value stack_tag
| E : frame stack_tag
| D : control stack_tag

and code_ptr = Code of int

and value = Value : 'a value_tag * 'a -> value
and _ value_tag =
| Int : int value_tag
| Pair : (value * value) value_tag
| Closure : (code_ptr * environment) value_tag
| Dummy : dummy value_tag

and control = Control : 'a control_tag * 'a -> control
and _ control_tag =
| Join : code_ptr control_tag
| Ret : (code_ptr * environment) control_tag
| Stop : unit control_tag

type code = instruction array
and instruction = unit

type ('a, 'b) tag_error = { expected: 'a; found : 'b; }

type error =
    | Empty_stack : 'a stack_tag -> error
    | Invalid_frame_index
    | Value_tag_mismatch
        : ('a value_tag, 'b value_tag) tag_error -> error
    | Control_tag_mismatch
        : ('a control_tag, 'b control_tag) tag_error -> error
    | Division_by_zero
    | Dummy_frame_altered

exception Machine_stop
exception Error of error

let error err = raise (Error err)
let value_tag_error err =
  raise (Error (Value_tag_mismatch err))
let control_tag_error err =
  raise (Error (Control_tag_mismatch err))

let tag : type a . a value_tag -> a -> value =
  fun ty v -> match ty with
    | Int -> Value (Int, v)
    | Pair -> Value (Pair, v)
    | Closure -> Value (Closure, v)
    | Dummy -> Value (Dummy, v)

let untag : type a . a value_tag -> value -> a =
  fun ty v -> match ty, v with
    | Int, Value (Int, n) -> n
    | Pair, Value (Pair, p) -> p
    | Closure, Value (Closure, ptr) -> ptr
    | Dummy, Value (Dummy, Dummy) -> Dummy
    | expected, Value (found, _) ->
      value_tag_error { expected; found; }

let control_tag ty v = Control (ty, v)

let control_untag : type a . a control_tag -> control -> a =
 fun ty v -> match ty, v with
   | Join, Control (Join, v) -> v
   | Ret, Control (Ret, v) -> v
   | Stop, Control (Stop, v) -> v
   | expected, Control (found, _) ->
     control_tag_error { expected; found; }

let next_instr (Code n) = Code (n + 1)

let aux_push v {s;e;c;d} =
  let s = v::s in
  let c = next_instr c in
  {s;e;c;d}

let pop : type a . a stack_tag -> a list -> a * a list =
 fun tag -> function
  | [] -> error (Empty_stack tag)
  | x::stack -> (x, stack)

let nth : type a . a stack_tag -> a list -> int -> a =
 fun tag stack n ->
   try List.nth stack n
   with _ -> error (Empty_stack tag)

let push n reg = aux_push (tag Int n) reg

let frame_get frame i =
  try frame.(i)
  with _ -> error Invalid_frame_index

let ld n i reg =
  let frame = nth E reg.e n in
  let v = frame_get frame i in
  aux_push v reg

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

let add reg = binop (intop (+)) reg
let sub reg = binop (intop (-)) reg
let mul reg = binop (intop ( * )) reg
let div reg =
  let div a b =
    if b = 0 then error Division_by_zero
    else a / b in
  binop (intop div) reg

let cmpop cmp = intop (fun x y -> if cmp x y then 1 else 0)
let ceq reg = binop (cmpop (=))
let cgt reg = binop (cmpop (>))
let cgte reg = binop (cmpop (>=))

let atom reg =
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

let sel t f {s;e;c;d} =
  let x, s = pop S s in
  let b = untag Int x in
  let d = control_tag Join (next_instr c) :: d in
  let c = if b = 0 then f else t in
  {s;e;c;d}

let join {s;e;c;d} =
  let x, d = pop D d in
  let c = control_untag Join x in
  {s;e;c;d}

let ldf f {s;e;c;d} =
  let s = tag Closure (f,e) :: s in
  let c = next_instr c in
  {s;e;c;d}

let ap n {s;e;c;d} =
  let x, s = pop S s in
  let (f, e) = untag Closure x in
  let frame, s =
    let frame = Array.make n (tag Dummy Dummy) in
    let cur_s = ref s in
    for i = n - 1 downto 0 do
      let x, s = pop S !cur_s in
      frame.(i) <- x;
      cur_s := s;
    done;
    frame, !cur_s in
  (* a difference from the spec:
     Ret control slots keep both
     the code pointer and the environment,
     instead of storing them as two separate stack slots *)
  let d = control_tag Ret (next_instr c, e) :: d in
  let e = frame :: e in
  let c = f in
  {s;e;c;d}

let rtn {s;e;c;d} =
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
  (* difference from the spec: we represent a dummy frame
     as a frame of dummy values *)
  let frame = Array.make n (tag Dummy Dummy) in
  let e = frame :: e in
  let c = next_instr c in
  {s;e;c;d}

let check_dum frame n =
  if Array.length frame <> n then
    error Dummy_frame_altered;
  Array.iter (function (Value (tag, _)) ->
    match tag with
      | Dummy -> ()
      | _ -> error Dummy_frame_altered
  ) frame

let rap n {s;e;c;d} =
  let x,s = pop S s in
  let (f, fp) = untag Closure x in
  let frame, fpp = pop E fp in
  check_dum frame n;
  let s =
    let cur_s = ref s in
    for i = n-1 downto 0 do
      let y,s = pop S !cur_s in
      cur_s := s;
      frame.(i) <- y;
    done;
    !cur_s in
  let d = control_tag Ret (next_instr c, fpp) :: d in
  let e = fp in
  let c = f in
  {s;e;c;d}

let stop = raise Machine_stop

