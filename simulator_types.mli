module type MAP =
  sig
    val width : int
    val height : int
    val data : Map.t
    val lambda_man_start : int * int
    val ghosts_start : (int * int) array
  end
module L :
  sig
    type t = {
      mutable x : int;
      mutable y : int;
      mutable tick_to_move : int;
      mutable lives : int;
    }
  end
module G :
  sig
    type t = {
      ghc : Ghc.state;
      mutable x : int;
      mutable y : int;
      mutable tick_to_move : int;
      mutable direction : int;
      mutable vitality : int;
      index : int;
      program_index : int;
    }
  end
type state = {
  ghosts : G.t array;
  lambda_man : L.t;
  mutable tick : int;
  mutable pills : int;
  mutable fright_mode : int option;
}
