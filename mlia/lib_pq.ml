type priority = int
type 'a priority_queue =
    Empty
  | Node of (priority * ('a * ('a priority_queue * 'a priority_queue)))

let pq_empty = Empty

let rec pq_insert queue prio elt =
  match queue with
    Empty -> Node(prio, (elt, (Empty, Empty)))
  | Node(p, (e, (left, right))) ->
     if prio <= p
     then Node(prio, (elt, (pq_insert right p e, left)))
     else Node(p, (e, (pq_insert right prio elt, left)))

let (>>=) e f =
  match e with
  | None -> None
  | Some e -> f e

let rec pq_remove_top = function
  | Empty -> None
  | Node(prio, (elt, (left, Empty))) -> Some left
  | Node(prio, (elt, (Empty, right))) -> Some right
  | Node(prio, (elt, ((Node(lprio, (lelt, (_, _))) as left),
		      (Node(rprio, (relt, (_, _))) as right)))) ->
     if lprio <= rprio
     then
       pq_remove_top left >>= fun left ->
       Some (Node(lprio, (lelt, (left, right))))
     else
       pq_remove_top right >>= fun right ->
       Some (Node(rprio, (relt, (left, right))))

let pq_extract_top = function
  | Empty -> None
  | Node(prio, (elt, (_, _))) as queue ->
     pq_remove_top queue >>= fun q ->
     Some (prio, elt, q)
