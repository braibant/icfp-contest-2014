type ('b, 'c) int_or_pair =
| Foo0
| Foo1
| Foo2 of 'b
| Foo3 of 'c

external left : int -> ('b, 'c) int_or_pair = "gcc_left"
external right : ('b * 'c) -> ('b, 'c) int_or_pair = "gcc_right"

external case
  : ('b, 'c) int_or_pair -> (int -> 'a) -> ('b * 'c -> 'a) -> 'a
  = "gcc_case"

(*
external case_const
  : ('b, 'c) int_or_pair -> 'a -> ('b * 'c -> 'a) -> 'a
  = "gcc_case_const"
*)
let case_const
    : ('b, 'c) int_or_pair -> 'a -> ('b * 'c -> 'a) -> 'a
  = fun data left right ->
    case data (fun _ -> left) right

type 'a option =
| Some of 'a
| None

type 'a flist = ('a, 'a flist) int_or_pair
let nil : 'a flist = left 0
let cons x xs = right (x, xs)

let rec fold_left f acc li =
  case_const li acc
    (fun x xs -> fold_left (f acc x) xs)

type 'a status =
| Continue of 'a
| Stop of 'a

let rec fold_left_stop f acc li =
  case_const li acc
    (fun x xs ->
      match f acc x with
        | Stop v -> v
        | Continue acc -> fold_left_stop f acc xs)
