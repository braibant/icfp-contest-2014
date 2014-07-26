let test file =
  let e = Ghc_misc.of_file file in
  Format.printf "===%s===\n%a" file Ghc_misc.pp e

let _ = List.iter test ["miner.ghc"; "flipper.ghc"; "frickle.ghc"]
