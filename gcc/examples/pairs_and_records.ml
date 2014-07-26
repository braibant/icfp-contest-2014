let test1 = (3,4,5,6)
let test2 = (3, 4)

type t = { a : int; b : int; c : int * int; d : int * int * int }
let test3 = { a = 1; b = 2; c = (3, 4); d = (5, 6, 7) }

let test4 (x, y, z) = (z+3, y+2, x+1)
let test5 t = t.a + (let (_,_,b) = t.d in b)
