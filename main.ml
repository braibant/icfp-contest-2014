
open Cmdliner

let main map ghosts lambdamen =
  ()


let ghosts =
  let doc = "Declare the location of ghost program" in
  Arg.(non_empty & opt_all file [] & info ["ghc"] ~doc)

let lambdamen =
  let doc = "Declare the location of the gcc files" in
  Arg.(non_empty & opt_all file [] & info ["gcc"] ~doc)

let map =
  let doc = "Set the map file" in
  Arg.(required & opt (some string) None & info ["m"; "map"] ~doc)

let term  =
  Term.(pure main $ map $ ghosts $ lambdamen),
  Term.info "main" ~doc:"simulates the execution of a game"


let () = match Term.eval term with `Error _ -> exit 1 | _ -> exit 0
