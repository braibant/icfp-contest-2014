open Simulator_types;;
open Display;;

let test () =
  let b = Board.of_lines [
      "##############";
      "#.# #o# # #%\\#";
      "#            #";
      "#            #";
      "#            #";
      "#............#";
      "##############";
    ]
  in
  let g0 = G.({
    ghc = Ghc.init 0 [| |];
    x = 1;
    y = 0;
    tick_to_move = 0;
    direction = 0;
    vitality = 0;
    index = 0;
    program_index = 0;
  }) in
  let ghosts = Array.init 2 (fun i -> G.({g0 with y = i+3; index=i})) in
  let man = L.({
    x = 1;
    y = 2;
    tick_to_move = 0;
    direction = 0;
    lives = 3;
  }) in
  let state = {
    ghosts = ghosts;
    lambda_man = man;
    tick = 0;
    pills = 0;
    fright_mode = None;
  } in
  init b;
  ignore (Graphics.wait_next_event [Graphics.Key_pressed]);
  Board.set b 12 1 Content.Empty;
  for x = 1 to 12 do
    for i = 0 to Array.length ghosts - 1 do ghosts.(i).G.x <- x done;
    man.L.x <- x;
    show b state;
    ignore (Graphics.wait_next_event [Graphics.Key_pressed]);
  done;
;;

test();;
