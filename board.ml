type t =
    {
      board: Content.t array array;
      lambda_man_start: int * int;
      ghosts_start: (int * int) array;
      fruit_position: int * int
    }

let get map  ~x ~y =
  map.board.(x).(y)

let set map  ~x ~y c =
  map.board.(x).(y) <- c

let width t = Array.length t.board
let height t = Array.length t.board.(0)
let ghosts_start t = t.ghosts_start
let lambda_man_start t = t.lambda_man_start
let fruit_position t = t.fruit_position

let iter f map =
  for x = 0 to width map - 1 do
    for y = 0 to height map - 1 do
      f ~x ~y map.board.(x).(y)
    done
  done

let of_lines lines : t =
  let lines = Array.of_list lines in
  let columns = String.length lines.(0) in
  let board = Array.make_matrix columns (Array.length lines) (Content.of_char ' ') in
  let lman = ref None in
  let ghosts = ref [] in
  let fruit = ref None in
  Array.iteri
    (fun y line ->
      String.iteri (fun x c ->
        let c = Content.of_char c in
        board.(x).(y) <-  c;
        match c with
          | Content.LambdaManStart -> lman := Some (x,y);
          | Content.GhostStart -> ghosts := (x,y)::! ghosts;
          | Content.Fruit -> fruit := Some (x,y)
          | _ -> ()
      ) line
    ) lines;
  let lambda_man_start =
    match !lman  with
      | None -> assert false
      | Some x -> x
  in
  let ghosts_start =
    Array.of_list
      (List.sort (fun (x1,y1) (x2,y2) ->
        match compare y1 y2 with
          | 0 -> compare x1 x2
          | i -> i
       ) !ghosts)
  in
  let fruit_position =
    match !fruit  with
      | None -> assert false
      | Some x -> x
  in
  {
    board;
    lambda_man_start;
    fruit_position;
    ghosts_start;
  }

let of_file file =
  let i = open_in file in
  let lines =
    let l = ref [] in
    try
      while true do
        l := input_line i :: !l
      done;
      assert false
    with End_of_file ->
      List.rev !l
  in
  of_lines lines
