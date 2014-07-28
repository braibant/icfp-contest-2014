#mod_use "world.ml";;
open World

#mod_use "world_ia.ml";;
open World_ia;;

let map =
  [
    [Wall ; Wall ; Wall ; Wall ; Wall ;];
    [Wall ; Wall ; Empty; Wall ; Wall ;];
    [Wall ; Empty; Empty; Empty; Wall ;];
    [Wall ; Wall ; Empty ; Wall ; Wall ;];
    [Wall ; Wall ; Empty ; Wall ; Wall ;];
    [Wall ; Wall ; Wall ; Wall ; Wall ;];
  ]
(*
# bfs map [] (2,2);;
- : direction option = Some Down
*)
