open! Lib

let rec list_nth n = function
  | [] -> None
  | x::xs ->
    if n = 0 then Some x
    else list_nth (n-1) xs

let rec list_nth' n = function
  | [] -> assert false
  | x::xs ->
    if n = 0 then x
    else list_nth' (n-1) xs

let rec list_find p = function
  | [] -> None
  | x::xs ->
    if p x then Some x
    else list_find p xs

let rec list_mem
    (eq: 'a -> 'a -> bool)
    (x:'a) : 'a list -> bool
  = function
  | [] -> false
  | y :: xs ->
    eq x y || list_mem eq x xs


type iter_status =
| Continue
| Stop

let rec list_fold_left f acc = function
  | [] -> acc
  | x::xs -> list_fold_left f (f acc x) xs

let rec list_fold_left_stop f acc = function
  | [] -> Continue, acc
  | x::xs ->
    match f acc x with
      | Stop, v -> Stop, v
      | Continue, acc -> list_fold_left_stop f acc xs

let rec list_map f = function
  | [] -> []
  | x::xs ->
    let a = f x in a :: list_map f xs

let list_length li =
  let rec loop acc = function
  | [] -> acc
  | _::li -> loop (acc+1) li
  in loop 0 li

let rec list_rev_append l acc =
  match l with
    | [] -> acc
    | t::q -> list_rev_append q (t::acc)

let list_rev l = list_rev_append l []

let rec list_rev_append_map f li =
  match li with
    | [] -> []
    | x::xs ->
      list_rev_append (f x) (list_rev_append_map f xs)



(* instances *)
let mem_pos direction li = list_mem eq_pos direction li
let mem_dir direction li = list_mem eq_dir direction li
