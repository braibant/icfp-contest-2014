module DATA = struct
  type ('b, 'c) int_or_pair

  external left : int -> ('a, 'b, 'c) pair_or_int = "gcc_left"
  external right : ('b * 'c) -> ('b, 'c) pair_or_int = "gcc_right"

  external case
    : ('b, 'c) pair_or_int -> (int -> 'a) -> ('b * 'c -> 'a) -> 'a
    = "gcc_case"

(*
  external case_const
    : ('b, 'c) pair_or_int -> 'a -> ('b * 'c -> 'a) -> 'a
    = "gcc_case_const"
*)
  let case_const :
      : ('b, 'c) pair_or_int -> 'a -> ('b -> 'c -> 'a) -> 'a
    = fun data left right ->
      case data (fun _ -> left) right
end
open DATA

type 'a option =
| Some of 'a
| None

module LIST = struct
  type 'a t = ('a, 'a t) pair_or_int
  let nil : 'a flist = left 0
  let cons x xs = right x xs

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
          | Continue acc -> fold_left acc xs)
end

module MAP = struct
  type world = {
    map : map;
    lambda : lambda_status;
    ghosts : ghost_status LIST.t;
    fruits : fruit_status;
  }

  and map = square LIST.t LIST.t

  and square =
  | (* 0 *) Wall
  | (* 1 *) Empty
  | (* 2 *) Pill
  | (* 3 *) Power_pill
  | (* 4 *) Fruit
  | (* 5 *) Lambda_start
  | (* 6 *) Ghost_start

  and direction =
  | (* 0 *) Up
  | (* 1 *) Right
  | (* 2 *) Down
  | (* 3 *) Left

  and lambda_status = {
    vitality : int;
    loc : location;
    dir : direction;
    lives : int;
    score : int;
  }

  and ghost_status = {
    vitality : ghost_vitality;
    loc : location;
    dir : direction;
  }

  and fruit_status = int

  and location = int * int
end
