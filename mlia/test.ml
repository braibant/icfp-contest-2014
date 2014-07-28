#mod_use "world.ml";;
open World

#mod_use "world_ffi.ml";;
open World_ffi;;

let of_char = function
  | '-' -> Empty
  | 'X' -> Wall
  | 'o' -> Pill
  | 'O' -> Power_pill
  | _ -> invalid_arg "of_char"

let list_of_string str =
  let li = ref [] in
  for i = String.length str - 1 downto 0 do
    li := str.[i] :: !li;
  done;
  !li

let map = List.map (fun str -> List.map of_char (list_of_string str))
  [
       (*0123456789012345 *)
(* 0 *) "XXXXXXXXXXXXXXXX";
(* 1 *) "XXXXXXX--XXXXXXX";
(* 2 *) "XXXXXXXX-XXXXXXX";
(* 3 *) "XXXXXXXX---XXXXX";
(* 4 *) "XXXXXXXX-XXXXXXX";
(* 5 *) "XXXXXXXX-XXXXXXX";
(* 6 *) "XXXXXX-----XXXXX";
(* 7 *) "XXXXXXX-o-XXXXXX";
(* 8 *) "XXXXXXXXXXXXXXXX";
  ]

let map_start_pos = (7,1)
