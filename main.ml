
open Cmdliner

let main map ghosts lambdaman use_graphics =
  let board = Board.of_file map in
  let ghosts = Array.of_list ghosts |> Array.map Ghc_misc.of_file in
  let module S = Simulator.Make
                   (struct
                     let board = board
                     let ghost_programs = ghosts
                     let lambda_program =
		       Array.of_list (Gcc_lexer.program lambdaman)
                     let use_graphics = use_graphics
                   end)
  in
  S.repl ()


let ghosts =
  let doc = "Declare the location of ghost program" in
  Arg.(non_empty & opt_all file [] & info ["ghc"] ~doc)

let lambdaman =
  let doc = "Declare the location of the gcc file" in
  Arg.(required & opt (some file) None & info ["gcc"] ~doc)

let map =
  let doc = "Set the map file" in
  Arg.(required & opt (some string) None & info ["m"; "map"] ~doc)

let use_graphics =
  let doc = "Use graphics rendering (default)" in
  let graphics = true, Arg.info ["graphics"] ~doc in
  let doc = "Do not render graphics" in
  let no_graphics = false, Arg.info ["no-graphics"] ~doc in
  Arg.(last & vflag_all [true] [graphics; no_graphics])

let term  =
  Term.(pure main $ map $ ghosts $ lambdaman $ use_graphics),
  Term.info "main" ~doc:"simulates the execution of a game"


let () = match Term.eval term with `Error _ -> exit 1 | _ -> exit 0
