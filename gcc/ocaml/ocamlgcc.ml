open Misc
open Config
open Format

let init_path () =
  let dirs =
    if !Clflags.use_threads then "+threads" :: !Clflags.include_dirs
    else if !Clflags.use_vmthreads then "+vmthreads" :: !Clflags.include_dirs
    else !Clflags.include_dirs in
  let exp_dirs =
    List.map (expand_directory Config.standard_library) dirs in
  load_path := "" :: List.rev_append exp_dirs (Clflags.std_include_dir ());
  Env.reset_cache ()

(* Note: do not do init_path() in initial_env, this breaks
   toplevel initialization (PR#1775) *)
let initial_env () =
  Ident.reinit();
  try
    if !Clflags.nopervasives
    then Env.initial
    else Env.open_pers_signature "Pervasives" Env.initial
  with Not_found ->
    fatal_error "cannot open pervasives.cmi"


(* Note: this function is duplicated in optcompile.ml *)
let check_unit_name ppf filename name =
  try
    begin match name.[0] with
    | 'A'..'Z' -> ()
    | _ ->
       Location.print_warning (Location.in_file filename) ppf
        (Warnings.Bad_module_name name);
       raise Exit;
    end;
    for i = 1 to String.length name - 1 do
      match name.[i] with
      | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '\'' -> ()
      | _ ->
         Location.print_warning (Location.in_file filename) ppf
           (Warnings.Bad_module_name name);
         raise Exit;
    done;
  with Exit -> ()
;;

(* Compile a .ml file *)

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let (++) x f = f x

let implementation ppf sourcefile outputprefix =
  Location.input_name := sourcefile;
  init_path ();
  let modulename =
    String.capitalize(Filename.basename(chop_extension_if_any sourcefile)) in
  check_unit_name ppf sourcefile modulename;
  Env.set_unit_name modulename;
  let inputfile = Pparse.preprocess sourcefile in
  let env = initial_env() in
  let instrs =
    Pparse.file ppf inputfile Parse.implementation ast_impl_magic_number
    ++ print_if ppf Clflags.dump_parsetree Printast.implementation
    ++ print_if ppf Clflags.dump_source Pprintast.structure
    ++ Typemod.type_implementation sourcefile outputprefix modulename env
    ++ Translmod.transl_implementation modulename
    ++ print_if ppf Clflags.dump_rawlambda Printlambda.lambda
    ++ Simplif.simplify_lambda
    ++ print_if ppf Clflags.dump_lambda Printlambda.lambda
    ++ Compilegcc.compile_implementation modulename in
  Warnings.check_fatal ();
  Pparse.remove_preprocessed inputfile;
  instrs

let () =
  let print = ref false in
  let exec  = ref true in
  let files = ref [] in
    Arg.parse
      ["--print",Arg.Set print,"print the asm";
       "--dlambda",Arg.Set Clflags.dump_lambda,"print the parsetree";
       "--no-exec",Arg.Clear exec,"execute the asm";
      ]
      (fun f -> files := f :: !files)
      "Usage: miniml [-n] [file] ..." ;
    try
      List.iter (fun f ->
          let instrs = implementation Format.err_formatter f f in
          if !print then
            Format.printf "%a@." Gcc_instr.print_instructions instrs;
          if !exec  then
            let res = Gcc.run (Array.of_list instrs) Gcc.init_regs in
            Format.printf "res:%a@."
              (Pp.print_list Pp.semi Gcc.print_value) res.s
        ) !files;
      exit 0
    with
    | Compilegcc.Not_implemented e ->
      Format.eprintf "Not implemented lambda: %a@." Printlambda.lambda e;
      exit 2
    | x ->
      Errors.report_error Format.err_formatter x;
      exit 2
