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

and code_ptr = Code of int

and value = Value : 'a value_tag * 'a -> value
and _ value_tag =
| Int : int value_tag
| Pair : (value * value) value_tag
| Closure : (code_ptr * environment) value_tag

and control = control_tag * code_ptr
and control_tag =
| Join
| Ret
| Stop

type code = instruction array
and instruction = ()

type ('a, 'b) tag_error = { expected: 'a; found : 'b; }

type error =
    | E_overflow
    | D_overflow
    | S_overflow
    | Invalid_frame_index
    | Value_tag_mismatch
        : ('a value_tag, 'b value_tag) tag_error -> error
    | Control_tag_mismatch 
        : (control_tag, control_tag) tag_error -> error
    | Division_by_zero

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

let untag : type a . a value_tag -> value -> a =
  fun ty v -> match ty, v with
    | Int, Value (Int, n) -> n
    | Pair, Value (Pair, p) -> p
    | Closure, Value (Closure, ptr) -> ptr
    | expected, Value (found, _) -> 
      value_tag_error { expected; found; }

let control_tag ty v = (ty, v)

let control_untag ty v = match ty, v with
  | Join, (Join, ptr) -> ptr
  | Ret, (Ret, ptr) -> ptr
  | Stop, (Stop, ptr) -> ptr
  | expected, (found, _) ->
    control_tag_error { expected; found; }

let next_instr (Code n) = Code (n + 1)

let aux_push v {s;e;c;d} =
  let s = v::s in
  let c = next_instr c in
  {s;e;c;d}

let push n reg = aux_push (tag Int n) reg

let ld n i reg =
  let frame =
    try List.nth reg.e n
    with _ -> error E_overflow
  in
  let v =
    try frame.(i)
    with _ -> error Invalid_frame_index
  in
  aux_push v reg

exception Data_stack_overflow

let unop op {s;e;c;d} =
  match s with
    | [] -> error S_overflow
    | x::s ->
      let s = op x :: s in
      let c = next_instr c in
      {s;e;c;d}

let binop op {s;e;c;d} =
  match s with
    | [] | [_] -> error S_overflow
    | y :: x :: s ->
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
  match s with
    | [] -> error S_overflow
    | x::s ->
      let b = untag Int x in
      let d = (Join, next_instr c) :: d in
      let c = if b = 0 then f else t in
      {s;e;c;d}

let join {s;e;c;d} =
  match d with
    | [] -> error D_overflow
    | x::d ->
      let c = control_untag Join x in
      {s;e;c;d}

let ldf f {s;e;c;d} =
  let s = tag Closure (f,e) :: s in
  let c = next_instr c in
  {s;e;c;d}
