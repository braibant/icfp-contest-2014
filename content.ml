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
