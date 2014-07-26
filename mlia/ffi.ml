
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

type ('a,'b) pair
external pair: 'a -> 'b -> ('a,'b) pair = "gcc_right"
external case_pair_aux:
  ('a,'b) pair -> (int -> 'c) -> ('a -> 'b -> 'c) -> 'c = "gcc_case"

let case_pair p f = case_pair_aux p (fun _ -> assert false) f

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

let rec flist_of_list = function
  | [] -> nil
  | a::l -> cons a (flist_of_list l)

let f i x = let r = i*x in if r > 10 then Stop(r) else Continue r

let main_gcc = fold_left_stop f 1 (flist_of_list [1;2;3;4;5])
