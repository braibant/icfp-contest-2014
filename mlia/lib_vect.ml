open! Lib_list

type 'a vect_tree = Tree : ('a, 'b) vect_tag * 'b -> 'a vect_tree
and ('a, _) vect_tag =
| Leaf : ('a, 'a) vect_tag
| Node : ('a, 'a vect_tree * 'a vect_tree) vect_tag
type 'a vect = 'a vect_tree * int

let get_vect (t, n) i =
  let rec find : type a . int -> int -> a vect_tree -> a =
    fun i n -> function
    | Tree (Leaf, v) -> v
    | Tree (Node, (left, right)) ->
      let mid = n / 2 in
      if i < mid
      then find i mid left
      else find (i - mid) (n - mid) right
  in find i n t

let rec vect_of_list li =
  let rec consume li n =
    if n = 1 then
      begin match li with
        | [] -> assert false
        | x::xs -> (Tree (Leaf, x), xs)
      end
    else begin
      let mid = n / 2 in
      let left, li = consume li mid in
      let right, li = consume li (n - mid) in
      Tree (Node, (left, right)), li
    end
  in
  let len = list_length li in
  match consume li len with
    | tree, [] -> (tree, len)
    | _, _::_ -> assert false

let list_of_vect =
  let rec prepend : type a . a vect_tree -> a list -> a list =
  fun tree acc -> match tree with
  | Tree (Leaf, v) -> v::acc
  | Tree (Node, (left, right)) ->
    prepend left (prepend right acc)
  in fun (tree, _n) -> prepend tree []

let map_vect f ((tree, n) : 'a vect) =
  let rec map : type a b . (a -> b) -> a vect_tree -> b vect_tree =
    fun f -> function
      | Tree (Leaf, v) -> Tree (Leaf, f v)
      | Tree (Node, (left, right)) ->
        Tree (Node, (map f left, map f right))
  in (map f tree, n)
