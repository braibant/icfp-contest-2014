
(* Usage: ghc_trace <board> <ghc> *)

let main () =
  let initboard = Board.of_file Sys.argv.(1) in
  let code = Ghc_misc.of_file Sys.argv.(2) in
  let env =
    Ghc.({ lman_coordinates = [|Board.lambda_man_start initboard; (0, 0)|];
           ghost_starting_positions = Board.ghosts_start initboard;
           ghost_current_positions = Board.ghosts_start initboard;
           ghost_stats =
             Array.map (fun _ -> (0, 0)) (Board.ghosts_start initboard);
           map = initboard;
          })
  in
  let state = Ghc.init 0 code in
  Ghc.execute ~debug:true env state;
;;

main ();;
