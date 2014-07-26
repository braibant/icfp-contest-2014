type t
val get : t -> x:int -> y:int -> Content.t
val set : t -> x:int -> y:int -> Content.t -> unit
val width : t -> int
val height : t -> int
val ghosts_start : t -> (int * int) array
val lambda_man_start : t -> int * int
val fruit_position : t -> int * int
val pills : t -> int
val iter : (x:int -> y:int -> Content.t -> 'a) -> t -> unit
val of_lines : string list -> t
val of_file : string -> t
