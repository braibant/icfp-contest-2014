
type 'a flist = 'a list

let cons x l = x::l
let nil = []

let case_const
  : 'b flist -> 'a -> ('b -> 'b flist -> 'a) -> 'a
  = fun data left right ->
    match data with
    | [] -> left
    | a::l -> right a l

let rec fold_left (f:'a -> 'b -> 'a) (acc:'a) (li:'b flist) =
  case_const li acc
    (fun x xs -> fold_left f (f acc x) xs)

type 'a status =
| Continue
| Stop

let rec fold_left_stop f acc li =
  case_const li acc
    (fun x xs ->
      match f acc x with
        | Stop, v -> v
        | Continue, acc -> fold_left_stop f acc xs)

let rec flist_of_list = function
  | [] -> nil
  | a::l -> cons a (flist_of_list l)

let f i x = let r = i*x in if r > 10 then Stop,r else Continue,r

let main_gcc = fold_left_stop f 1 (flist_of_list [1;2;3;4;5])
