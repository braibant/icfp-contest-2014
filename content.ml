type t =
  | Wall
  | Empty
  | Pill
  | PowerPill
  | Fruit
  | LambdaMan
  | Ghost

let to_char = function
  | Wall -> '#'
  | Empty -> ' '
  | Pill -> '.'
  | PowerPill -> 'o'
  | Fruit -> '%'
  | LambdaMan -> '\\'
  | Ghost -> '='

let of_char = function
  |'#'  ->  Wall
  |' '  ->  Empty
  |'.'  ->  Pill
  |'o'  ->  PowerPill
  |'%'  ->  Fruit
  |'\\' ->  LambdaMan
  |'='  ->  Ghost
