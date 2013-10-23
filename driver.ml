open Index
open Generate

let create_package_directory () =
  let package_name = Opam_doc_config.current_package () in
  if not Sys.(file_exists package_name && is_directory package_name) then
    Unix.mkdir package_name 0o755

let get_cmt cmd cmt_list =
  let base = Filename.chop_extension cmd in
  let pSameBase cmt = (Filename.chop_extension cmt) = base in
  try
    Some (List.find pSameBase cmt_list)
  with Not_found -> None

let process_cmd cmd =
  Cmd_format.(
    try
      let cmd = read_cmd cmd in Some cmd.cmd_doctree
    with
	_ -> None
  )

let rec check_package_name_conflict global =
  let rec loop () =
    begin
      Printf.printf "Package '%s' already exists. Proceed anyway? [Y/n/r] \n%!"
                    (Opam_doc_config.current_package ());
      Scanf.scanf "%c" (function
        | 'Y' | 'y' | '\n' -> ()
        | 'N' | 'n' -> Printf.printf "Conflict unresolved. Exiting now..."; exit 0
        | 'r' ->
	    Printf.printf "New package name : ";
	    Opam_doc_config.set_current_package (read_line ());
	    check_package_name_conflict global
        | _ -> loop ())
    end
  in
  if Index.package_exists global (Opam_doc_config.current_package ()) 
     && not (Opam_doc_config.always_proceed ()) then loop ()

let process_file global cmd cmt =
  let module_name = String.capitalize
    (Filename.chop_extension (Filename.basename cmd)) in
  let doctree = process_cmd cmd in
  let cmi, cmt = Cmt_format.read cmt in
  match cmi, cmt with
    | _, None -> raise (Failure "Not a cmt file")
    | None, Some _ -> raise (Failure "I need the cmti")
    | Some cmi, Some cmt ->
      let imports = cmi.Cmi_format.cmi_crcs in
      let local = create_local global imports in
      Index.reset_internal_table ();
      try
	match cmt.Cmt_format.cmt_annots with
          | Cmt_format.Interface intf ->
	    Some (generate_file_from_interface local module_name doctree intf)
          | Cmt_format.Implementation impl ->
	    Some (generate_file_from_structure local module_name doctree impl)
          | _ -> raise (Failure "Wrong kind of cmt file")
      with
	| Invalid_argument s ->
	  Printf.eprintf "Error \"%s\" while processing module %s. File skipped\n%!" 
	    s module_name;
	  None

let _ =
  let files = ref [] in

  Opam_doc_config.(
    Arg.parse options (fun file -> files := file :: !files) usage
  );

  (* read the saved global table *)
  let global = read_global_file (Opam_doc_config.index_file_path ()) in
  
  check_package_name_conflict global;

  let global = add_global_package global
    (Opam_doc_config.current_package ())
    (Opam_doc_config.package_descr ()) in

  let cmt_files = List.filter
    (fun file -> Filename.check_suffix file ".cmti"
      || Filename.check_suffix file ".cmt") !files in

  let cmd_files = List.filter
    (fun file -> Filename.check_suffix file ".cmdi"
      || Filename.check_suffix file ".cmd") !files in

  (* Remove the [ext] file when a [ext]i is found *)
  let filter_impl_files ext files =
    let should_be_kept file =
      if Filename.check_suffix file ext then
	try
	  ignore (List.find ((=) ((Filename.chop_extension file)^ext^"i")) files);
	  false
	with Not_found -> true
      else true
	in
    List.filter should_be_kept files
  in

  let cmt_files = filter_impl_files ".cmt" cmt_files in
  let cmd_files = filter_impl_files ".cmd" cmd_files in

  (* Update the global table with the future processed cmts *)
  let global = update_global global cmt_files in

  create_package_directory ();

  let processed_files = 
    List.fold_left
      (fun l cmd -> 
         match get_cmt cmd cmt_files with
         | Some cmt -> begin match process_file global cmd cmt with
           | Some o -> o :: l
           | None -> assert false
         end
         | None ->
           prerr_endline ("Warning: missing cmt file: " ^ cmd);
           l)
      []
      cmd_files
  in
  let processed_files =
    List.sort (fun (x,_) (y,_) -> compare x y) processed_files
  in

  if processed_files != [] then
    begin
      let open Html_utils in
	  output_style_file ();
	  output_script_file ();
	  generate_package_index processed_files;
	  generate_global_packages_index global
    end;

  (* write down the updated global table *)
  write_global_file global (Opam_doc_config.index_file_path ())
