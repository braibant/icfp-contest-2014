type t =
  | Wall
  | Empty
  | Pill
  | PowerPill
  | Fruit
  | LambdaMan
  | Ghost

val to_char: t -> char
val of_char: char -> t
