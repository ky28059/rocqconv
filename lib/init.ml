let env_initialized = ref false

let init_rocq_env () =
  Unix.putenv "OCAMLFIND_CONF" "/dev/null"; (* Hack to prevent "Config file not found" *)
  Coqinit.init_ocaml ();

  let opts, _ = Coqargs.parse_args ~init:Coqargs.default [] in
  let usage = Boot.Usage.{
    executable_name = "rocqconv"; 
    extra_args = "";
    extra_options = ""
  } in
  Coqinit.init_runtime ~usage opts;
  Coqinit.init_document opts;

  let injections = Coqargs.injection_commands opts in
  let top = Names.DirPath.make [Names.Id.of_string "Top"] in

  env_initialized := true;

  Coqinit.start_library
    ~intern:Vernacinterp.fs_intern
    ~top
    injections
