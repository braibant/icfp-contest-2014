open Ffi

type ghost_vitality =
| (* 0 *) Standard
| (* 1 *) Fright_mode
| (* 2 *) Invisible

type square =
| (* 0 *) Wall
| (* 1 *) Empty
| (* 2 *) Pill
| (* 3 *) Power_pill
| (* 4 *) Fruit
| (* 5 *) Lambda_start
| (* 6 *) Ghost_start

type direction =
| (* 0 *) Up
| (* 1 *) Right
| (* 2 *) Down
| (* 3 *) Left

type location = int * int

type fruit_status = int

type lambda_status = {
  vitality : int;
  loc : location;
  dir : direction;
  lives : int;
  score : int;
}

type ghost_status = {
  vitality : ghost_vitality;
  loc : location;
  dir : direction;
}

type t = {
  map : map;
  lambda : lambda_status;
  ghosts : ghost_status flist;
  fruits : fruit_status;
}
and map = square flist flist


