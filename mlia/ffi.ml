
(*
most generic version

external left : int -> ('b, 'c) int_or_pair = "gcc_left"
external right : 'b -> 'c -> ('b, 'c) int_or_pair = "gcc_right"

external case
  : ('b, 'c) int_or_pair -> (int -> 'a) -> ('b -> 'c -> 'a) -> 'a
  = "gcc_case"
*)


type +'a flist
external mk_nil: int -> 'a flist = "gcc_left"
external cons: 'a -> 'a flist -> 'a flist = "gcc_right"

external case_flist: 'b flist -> (int -> 'a) -> ('b -> 'b flist -> 'a) -> 'a
  = "gcc_case"

let nil = mk_nil 0

(*
external case_const
  : ('b, 'c) int_or_pair -> 'a -> ('b -> 'c -> 'a) -> 'a
  = "gcc_case_const"
*)
let case_const
  : 'b flist -> 'a -> ('b -> 'b flist -> 'a) -> 'a
  = fun data left right ->
    case_flist data (fun _ -> left) right

let rec fold_left (f:'a -> 'b -> 'a) (acc:'a) (li:'b flist) =
  case_const li acc
    (fun x xs -> fold_left f (f acc x) xs)

type 'a status =
| Continue of 'a
| Stop of 'a

let rec fold_left_stop f acc li =
  case_const li acc
    (fun x xs ->
      match f acc x with
        | Stop v -> v
        | Continue acc -> fold_left_stop f acc xs)
