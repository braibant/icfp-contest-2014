type key = int * int

type color = Black | Red

type 'a rbt =
  | Empty
  | Node of color * 'a rbt * key * 'a * 'a rbt


let int_compare (a: int) b =
  compare a b

let key_compare ((a,b): key) ((c,d):key) =
  let cmp = int_compare a c in
  if cmp = 0
  then int_compare b d
  else cmp

let empty = Empty

let rec find (x: key) = function
  | Empty -> None
  | Node (_,l,k,v,r) ->
    begin
      match key_compare x k with
        | -1 -> find x l
        | 0 -> Some v
        | _ -> find x r
    end


let blacken = function
  | Node (_,l,k,v,r) -> Node (Black, l,k,v,r)
  | Empty -> Empty

let balance = function
  | Node (Black,(Node (Red,Node (Red,a, kx, vx, b), ky, vy, c)
                    | Node (Red,a, kx, vx, Node (Red, b, ky, vy, c))), kz, vz, d)
  | Node (Black,a, kx, vx, (Node (Red,Node (Red,b, ky, vy, c), kz, vz, d)
                               | Node (Red,b, ky, vy, Node (Red,c, kz, vz, d))))
    -> Node (Red,Node (Black, a, kx, vx, b), ky, vy, Node (Black,c, kz, vz, d))
  | n -> n

let insert k v n =
  let rec insert k v t = match t with
    | Empty -> Node (Red, Empty, k, v, Empty)
    | Node (color, l, k',v',r)  ->
      match key_compare k k' with
        | -1 -> let l,r = insert k v l, r in
                balance (Node (color, l, k', v',r ))
        | 1  -> let l, r = l, insert k v r in
                balance (Node (color, l, k', v',r ))
        | _  -> Node  (color, l, k,v, r)
  in blacken (insert k v  n)

let rec elements = function
  | Empty -> []
  | Node (_, l, k, v, r) ->
    elements l @ ((k,v)::elements r)
