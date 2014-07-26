type t = int 			(* 0 to 255 *)

let norm x =
  let r = x mod 256 in
  if r < 0
  then r + 256
  else r

let succ i   = norm (i + 1)
let pred i   = norm (i - 1)
let add  i j = norm (i + j)
let sub i j  = norm (i - j)
let mul i j  = norm (i * j)
let div i j  = norm (i / j)

let _land i j= i land j
let _lor i j = i lor j
let _lxor i j= i lxor j

let lt i j = i < j
let eq i j = i = j
let gt i j = i > j
