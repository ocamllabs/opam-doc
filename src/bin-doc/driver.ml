
open Config
open Clflags
open Compenv

let cmd_magic_number = "Caml2013E001"

type cmd = {
  cmd_modname: string;
  cmd_sourcefile: string;
  cmd_doctree: Doctree.file;
}

let save_cmd filename modname sourcefile doctree = 
  let cmd = 
    { cmd_modname = modname;
      cmd_sourcefile = sourcefile;
      cmd_doctree = doctree; }
  in
  let oc = open_out_bin filename in
    output_string oc cmd_magic_number;
    output_value oc (cmd : cmd);
    close_out oc

let read_file filename = 
  let chanin = open_in_bin filename in
  let len = 1024 in
  let s = String.create len in
  let buf = Buffer.create len in
  let rec iter () =
    try
      let n = input chanin s 0 len in
      if n = 0 then
        ()
      else
        (
         Buffer.add_substring buf s 0 n;
         iter ()
        )
    with
      End_of_file -> ()
  in
  iter ();
  close_in chanin;
  Buffer.contents buf

(** Initialize the search path.
   The current directory is always searched first,
   then the directories specified with the -I option (in command-line order),
   then the standard library directory. *)
let init_path () =
  load_path :=
    "" :: List.rev (Config.standard_library :: !Clflags.include_dirs);
  Env.reset_cache ()

(** Return the initial environment in which compilation proceeds. *)
let initial_env () =
  try
    if !Clflags.nopervasives
    then Env.initial
    else Env.open_pers_signature "Pervasives" Env.initial
  with Not_found ->
    Misc.fatal_error "cannot open pervasives.cmi"

(** Optionally preprocess a source file *)
let preprocess sourcefile =
  try
    Pparse.preprocess sourcefile
  with Pparse.Error _ ->
    Printf.eprintf "Preprocessing error\n";
    exit 2

type file_type =
    Impl_file
  | Intf_file

let print_version_and_library () =
  print_string "The OCaml compiler, version ";
  print_string Config.version; print_newline();
  print_string "Standard library directory: ";
  print_string Config.standard_library; print_newline();
  exit 0

let print_version_string () =
  print_string Config.version; print_newline(); exit 0

let print_standard_library () =
  print_string Config.standard_library; print_newline(); exit 0

let files = ref []

let anonymous f =
  if Filename.check_suffix f "ml" then
    files := !files @ [f, Impl_file]
  else if Filename.check_suffix f "mli" then
    files := !files @ [f, Intf_file]
  else
    ()

let impl f = files := !files @ [f, Impl_file];;

let intf f = files := !files @ [f, Intf_file];;

let usage = "Usage: bin-doc <options> <files>\nOptions are:"

let show_config () =
  Config.print_config stdout;
  exit 0;
;;

module Options = Main_args.Make_bytecomp_options (struct
  let set r () = r := true
  let unset r () = r := false
  let _a = set make_archive
  let _absname = set Location.absname
  let _annot = set annotations
  let _binannot = set binary_annotations
  let _c = set compile_only
  let _cc s = c_compiler := Some s
  let _cclib s = ccobjs := Misc.rev_split_words s @ !ccobjs
  let _ccopt s = first_ccopts := s :: !first_ccopts
  let _config = show_config
  let _compat_32 = set bytecode_compatible_32
  let _custom = set custom_runtime
  let _dllib s = dllibs := Misc.rev_split_words s @ !dllibs
  let _dllpath s = dllpaths := !dllpaths @ [s]
  let _g = set debug
  let _i () = print_types := true; compile_only := true
  let _I s = include_dirs := s :: !include_dirs
  let _impl = impl
  let _intf = intf
  let _intf_suffix s = Config.interface_suffix := s
  let _labels = unset classic
  let _linkall = set link_everything
  let _make_runtime () =
    custom_runtime := true; make_runtime := true; link_everything := true
  let _no_app_funct = unset applicative_functors
  let _noassert = set noassert
  let _nolabels = set classic
  let _noautolink = set no_auto_link
  let _nostdlib = set no_std_include
  let _o s = output_name := Some s
  let _output_obj () = output_c_object := true; custom_runtime := true
  let _pack = set make_package
  let _pp s = preprocessor := Some s
  let _ppx s = first_ppx := s :: !first_ppx
  let _principal = set principal
  let _rectypes = set recursive_types
  let _runtime_variant s = runtime_variant := s
  let _short_paths = unset real_paths
  let _strict_sequence = set strict_sequence
  let _thread = set use_threads
  let _vmthread = set use_vmthreads
  let _unsafe = set fast
  let _use_prims s = use_prims := s
  let _use_runtime s = use_runtime := s
  let _v = print_version_and_library
  let _version = print_version_string
  let _vnum = print_version_string
  let _w = (Warnings.parse_options false)
  let _warn_error = (Warnings.parse_options true)
  let _warn_help = Warnings.help_warnings
  let _where = print_standard_library
  let _verbose = set verbose
  let _nopervasives = set nopervasives
  let _dsource = set dump_source
  let _dparsetree = set dump_parsetree
  let _dtypedtree = set dump_typedtree
  let _drawlambda = set dump_rawlambda
  let _dlambda = set dump_lambda
  let _dinstr = set dump_instr
  let anonymous = anonymous

end)

let parse_args () =
  Arg.parse Options.list anonymous usage

(** Process the given file, according to its extension. *)
let process_file (file, ftype) =
  Location.input_name := file;
  init_path ();
  let prefixname = Filename.chop_extension file in
  let modulename = String.capitalize(Filename.basename prefixname) in
  Env.set_unit_name modulename;
  let inputfile = preprocess file in
  let dfile = (file, read_file file) in
  let doctree, cmd_name = 
    match ftype with
	Impl_file -> 
          let parsetree = 
            Pparse.file 
              Format.err_formatter 
              inputfile 
              Parse.implementation 
              ast_impl_magic_number 
          in
          Warnings.check_fatal ();
          let doctree = Inlinedoc.parse_implementation dfile parsetree in
          let cmd_name = prefixname ^ ".cmd" in
          Doctree.Dfile_impl doctree, cmd_name
      | Intf_file ->
        let parsetree = 
          Pparse.file 
            Format.err_formatter 
            inputfile 
            Parse.interface 
            ast_intf_magic_number 
        in
        Warnings.check_fatal ();
        let doctree = Inlinedoc.parse_interface dfile parsetree in
        let cmd_name = prefixname ^ ".cmdi" in
        Doctree.Dfile_intf doctree, cmd_name
  in
  (*
  (* debug *)
     Printdoctree.file 0 (Format.std_formatter) doctree;
  *)
  Pparse.remove_preprocessed inputfile;
  save_cmd cmd_name modulename file doctree

let _ =
  try
    parse_args ();
    List.iter process_file !files
  with Bindoc_errors.Error(e, loc) ->
    Bindoc_errors.report_error Format.err_formatter loc e
