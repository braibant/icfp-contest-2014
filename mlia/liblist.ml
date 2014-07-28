let rec nth n = function
  | [] -> None
  | x::xs ->
    if n = 0 then Some x
    else nth (n-1) xs

let rec nth' n = function
  | [] -> assert false
  | x::xs ->
    if n = 0 then x
    else nth' (n-1) xs

let rec find p = function
  | [] -> None
  | x::xs ->
    if p x then Some x
    else find p xs

let rec mem
    (eq: 'a -> 'a -> bool)
    (x:'a) : 'a list -> bool
  = function
  | [] -> false
  | y :: xs ->
    eq x y || mem eq x xs


type iter_status =
| Continue
| Stop

let rec fold_left f acc = function
  | [] -> acc
  | x::xs -> fold_left f (f acc x) xs

let rec fold_left_stop f acc = function
  | [] -> Continue, acc
  | x::xs ->
    match f acc x with
      | Stop, v -> Stop, v
      | Continue, acc -> fold_left_stop f acc xs


let rec list_map f = function
  | [] -> []
  | x::xs ->
    let a = f x in a :: list_map f xs

let list_length li =
  let rec loop acc = function
  | [] -> acc
  | _::li -> loop (acc+1) li
  in loop 0 li

let rec list_rev acc l =
  match l with
    | [] -> acc
    | t::q -> list_rev (t::acc) q

let list_rev l = list_rev [] l
