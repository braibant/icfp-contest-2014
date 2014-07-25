type t =
  | Wall
  | Empty
  | Pill
  | PowerPill
  | Fruit
  | LambdaManStart
  | GhostStart

val to_char: t -> char
val of_char: char -> t
