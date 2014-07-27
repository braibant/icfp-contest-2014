open Simulator_types;;

let s = 20;;  (* size of a tile (width and height) *)

let tiles = ref [| |];;

let width = ref 0;;
let height = ref 0;;
let cur = ref [| |];;

let draw_tile x y i =
  if !cur.(x).(y) <> i then begin
    !cur.(x).(y) <- i;
    Graphics.draw_image !tiles.(i) (x*s) ((!height-1 - y)*s);
  end;
;;

let draw_board board =
  let f ~x ~y t = draw_tile x y (Content.to_byte t) in
  Board.iter f board;
;;

let init board =
  width := Board.width board;
  height := Board.height board;
  cur := Array.init !width (fun _ -> Array.make !height 999);
  let w = s * !width in
  let h = s * !height in
  Graphics.open_graph (Printf.sprintf " %dx%d" w h);
  Graphics.auto_synchronize false;
  tiles :=
    Array.init 10 (fun i -> Graphics.make_image (Array.sub Tiles.raw (i*s) s));
  draw_board board;
;;

let close () = Graphics.close_graph ();;

let show board state =
  draw_board board;
  for i = 0 to Array.length state.ghosts - 1 do
    let g = state.ghosts.(i) in
    let open G in
    match g.vitality with
    | 0 -> draw_tile g.x g.y (6 + g.index mod 4);
    | 1 -> draw_tile g.x g.y (6 + g.index mod 4);
    | 2 -> draw_tile g.x g.y (6 + g.index mod 4);
    | _ -> assert false;
  done;
  let m = state.lambda_man in
  begin let open L in
    draw_tile m.x m.y 5;
  end;
  Graphics.synchronize ();
;;
