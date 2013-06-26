open Index
open Generate
open Cow

module StringMap = Map.Make(String)

let path = ref ""

type smap = string StringMap.t

let get_cmt cmd cmt_list =
  let base = Filename.chop_extension cmd in
  let pSameBase cmt = (Filename.chop_extension cmt) = base in
    try
      List.find pSameBase cmt_list
    with Not_found -> raise (Failure ("Missing cmt file: " ^ cmd))

let rec list_iter_between f o = function
  | [] -> ()
  | [h] -> f h
  | h::t -> f h; o (); list_iter_between f o t

let escape_string s =
  let buf = Buffer.create 80 in
    Buffer.add_string buf "\"";
    for i = 0 to String.length s - 1
    do
      let x =
        match s.[i] with
        | '\n' -> "\\n"
        | '\t' -> "\\t"
        | '\r' -> "\\r"
        | '\b' -> "\\b"
        | '\\' -> "\\\\"
        | '/' -> "\\/"
        | '"' -> "\\\""
        | '\x0c' -> "\\f"
        | c -> String.make 1 c
      in
        Buffer.add_string buf x
    done;
    Buffer.add_string buf "\"";
    Buffer.contents buf

let rec to_fct t f =
  match t with
  | Json.Int i -> f (Printf.sprintf "%Ld" i)
  | Json.Bool b -> f (string_of_bool b)
  | Json.Float r -> f (Printf.sprintf "%g" r)
  | Json.String s -> f (escape_string s)
  | Json.Null -> f "null"
  | Json.Array a ->
    f "[";
    list_iter_between (fun i -> to_fct i f) (fun () -> f ", \n") a;
    f "\n]";
  | Json.Object a ->
    f "{";
    list_iter_between (fun (k, v) -> to_fct (Json.String k) f; f ": "; to_fct v f)
      (fun () -> f ", \n") a;
    f "\n}"

let json_to_buffer t buf =
  to_fct t (fun s -> Buffer.add_string buf s)

let json_to_string json = 
  let buf = Buffer.create 2048 in
    json_to_buffer json buf;
    Buffer.contents buf

let process_cmd cmd =
  Cmd_format.(
    let cmd = read_cmd cmd in cmd.cmd_doctree
  )

let generate_json cmd jfile =
  let json = Docjson.json_of_file jfile in
  let json_name = (Filename.chop_extension cmd) ^ ".json" in
  let oc = open_out json_name in
  output_string oc (json_to_string json);
  close_out oc
    
let process_file global cmd cmt = 
  print_endline ("Processing : "^cmt^" and "^cmd);
  let module_name = String.capitalize 
    (Filename.chop_extension (Filename.basename cmd)) in
  let doc_file = process_cmd cmd in
  let cmi, cmt = Cmt_format.read cmt in
  match cmi, cmt with
    | _, None -> raise (Failure "Not a cmt file")
    | None, Some cmt -> raise (Failure "I need the cmti")
    | Some cmi, Some cmt ->
      let imports = cmi.Cmi_format.cmi_crcs in
      let local = create_local global imports in
      Index.reset_internal_references module_name;
      try 
	let jfile = 
	  match cmt.Cmt_format.cmt_annots with
            | Cmt_format.Interface intf ->   
	      generate_file_from_interface local doc_file intf
            | Cmt_format.Implementation impl ->  
	      generate_file_from_structure local doc_file impl
            | _ -> raise (Failure "Wrong kind of cmt file") 
	in
	(*generate_json cmd jfile;*)
	ignore(Doc_html.generate_html ~is_root_module:true module_name jfile);
      with
	| Invalid_argument s ->
	  Doc_html.print_error_page module_name;
	  Printf.eprintf "Error \"%s\". Module %s skipped\n%!" s module_name

let _ = 
  let global_path = ref "global.index"  in
  

  let files = ref [] in
  
  Arg.(
    parse
    [ ("-d", String(fun p -> path:=p; (*check "/" *) print_endline p), "")
    ; ("-index-file", Set_string (global_path), "Load a specific index file")
    ; ("-filter-pervasives", Set (Gentyp.filter_pervasives), "Removes 'Pervasives' ")
    ]
      (fun file -> files := file :: !files)
      "Bad usage");

  (* read the saved global table *)  
  let global = read_global_file !global_path in
(* 
   print_endline "[debug] global table before update :";
   global_print global;
*)

  let cmt_files = List.filter
    (fun file -> Filename.check_suffix file ".cmti"
      || Filename.check_suffix file ".cmt") !files in
  
  let cmd_files = List.filter 
    (fun file -> Filename.check_suffix file ".cmdi" 
      || Filename.check_suffix file ".cmd") !files in
    
  let filter_impl_files ext files = 
    List.filter 
      (fun file -> 
	if Filename.check_suffix file ext then
	  try ignore (List.find ((=) ((Filename.chop_extension file)^ext^"i")) files); 
	      false 
	  with Not_found -> true
	else true) 
      files 
  in

  let cmt_files = filter_impl_files ".cmt" cmt_files in  
  let cmd_files = filter_impl_files ".cmd" cmd_files in 

  (* updates the global table with the new references - 
     should also contain the possibles '-pack' cmt modules *)
  let global = update_global global cmt_files in

(*
  print_endline "[debug] global table after update :";
  global_print global;
*)
      
  List.iter 
    (fun cmd -> let cmt = get_cmt cmd cmt_files in
		try process_file global cmd cmt with e -> raise e)
    cmd_files;
  
  Doc_html.generate_module_index ();
  
  (* write down the updated global table *)
  write_global_file global !global_path
    
    
