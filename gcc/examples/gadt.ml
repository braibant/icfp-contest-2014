
(** Gabriel's encoding *)

(** François:
    Currently don't work, because of the reparsing but I don't know more. *)

(**
type 'a tree =
  | Empty
  | Leaf of 'a
  | Node of 'a tree * 'a tree
*)

type _ tree =
  | Empty : 'a tree
  | Other : ('a,'b) tree_tag * 'b -> 'a tree

and ('a,_) tree_tag =
| Leaf : ('a,'a) tree_tag
| Node : ('a,'a tree * 'a tree) tree_tag
