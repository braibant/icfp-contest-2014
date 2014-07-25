type t = int 			(* 0 to 255 *)
let succ i = (i + 1) mod 256
let pred i = (i - 1) mod 256
let add  i j = (i + j) mod 256
let sub i j = (i - j) mod 256
let mul i j = (i * j) mod 256
let div i j = (i / j) mod 256
let _land i j = i land j
let _lor i j = i lor j
let _lxor i j = i lxor j

let lt i j = i < j
let eq i j = i = j
let gt i j = i > j
