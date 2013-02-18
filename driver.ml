open Index
open Generate

module StringMap = Map.Make(String)

type smap = string StringMap.t

let get_cmt cmd cmt_list =
  let base = Filename.chop_extension cmd in
  let pSameBase cmt = (Filename.chop_extension cmt) = base in
    try
      List.find pSameBase cmt_list
    with Not_found -> raise (Failure ("Missing cmt file: " ^ cmd))

let process_file global cmd cmt = 
  let dintf = 
    let cmd = Cmd_format.read_cmd cmd in
      match cmd.Cmd_format.cmd_doctree with
        Doctree.Dfile_intf dintf -> dintf
      | Doctree.Dfile_impl _ -> raise (Failure "Not implemented")
  in
  let intf, imports = 
    let cmi, cmt = Cmt_format.read cmt in
      match cmi, cmt with
      | _, None -> raise (Failure "Not a cmt file")
      | None, Some cmt -> raise (Failure "Not implemented")
      | Some cmi, Some cmt -> 
          match cmt.Cmt_format.cmt_annots with
            Cmt_format.Interface intf -> intf, cmi.Cmi_format.cmi_crcs
          | Cmt_format.Implementation _ -> raise (Failure "Not implemented")
          | _ -> raise (Failure "Wrong kind of cmt file")
  in
    let local = create_local global imports in
    let jintf = generate_file local dintf intf in
    let json = Docjson.json_of_file jintf in
    let json_name = (Filename.chop_extension cmd) ^ ".json" in
    let oc = open_out json_name in
      output_string oc (Cow.Json.to_string json);
      close_out oc

let _ = 
  let files = 
    let files = ref [] in
      Arg.parse [] (fun file -> files := file :: !files) "Bad usage";
      !files
  in
  let cmt_files = List.filter (fun file -> Filename.check_suffix file ".cmti") files in
  let cmd_files = List.filter (fun file -> Filename.check_suffix file ".cmd") files in
  let global = create_global_from_files cmt_files in
    List.iter 
      (fun cmd -> let cmt = get_cmt cmd cmt_files in process_file global cmd cmt)
      cmd_files
