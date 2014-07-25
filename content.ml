type t =
  | Wall
  | Empty
  | Pill
  | PowerPill
  | Fruit
  | LambdaManStart
  | GhostStart


let to_char = function
  | Wall -> '#'
  | Empty -> ' '
  | Pill -> '.'
  | PowerPill -> 'o'
  | Fruit -> '%'
  | LambdaManStart -> '\\'
  | GhostStart -> '='

let of_char = function
  |'#'  ->  Wall
  |' '  ->  Empty
  |'.'  ->  Pill
  |'o'  ->  PowerPill
  |'%'  ->  Fruit
  |'\\' ->  LambdaManStart
  |'='  ->  GhostStart
  | _ -> assert false

let to_byte = function
  | Wall -> 0
  | Empty -> 1
  | Pill -> 2
  | PowerPill -> 2
  | Fruit -> 4
  | LambdaManStart -> 5
  | GhostStart -> 6
