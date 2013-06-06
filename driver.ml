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
    
let generate_html cmd jfile =
  Doc_html.(
    let module_name = String.capitalize 
      (Filename.chop_extension (Filename.basename cmd)) in
    let html = html_of_file module_name jfile in
    let html_name = module_name ^ ".html" in
    let oc = open_out html_name in
    output_string oc doctype;
    output_string oc (Docjson.string_of_html html);
    close_out oc;
  )
  
let process_file global cmd cmt = 
  let doc_file = process_cmd cmd in
  let cmi, cmt = Cmt_format.read cmt in
  match cmi, cmt with
    | _, None -> raise (Failure "Not a cmt file")
    | None, Some cmt -> raise (Failure "I need the cmti")
    | Some cmi, Some cmt ->
      let imports = cmi.Cmi_format.cmi_crcs in
      let local = create_local global imports in
      let jfile = 
	match cmt.Cmt_format.cmt_annots with
          | Cmt_format.Interface intf ->   
	    generate_file_from_interface local doc_file intf
          | Cmt_format.Implementation impl ->  
	    generate_file_from_structure local doc_file impl
          | _ -> raise (Failure "Wrong kind of cmt file") 
      in
      generate_json cmd jfile;
      generate_html cmd jfile

let _ = 
  let files = 
    let files = ref [] in
    Arg.(parse
    [( "-d", String(fun p -> path:=p; (*check "/" *) print_endline p), "osef")]
      (fun file -> files := file :: !files)
      "Bad usage");
    !files
  in
  let cmt_files = List.filter (fun file -> Filename.check_suffix file ".cmti" || Filename.check_suffix file ".cmt") files in
  let cmd_files = List.filter (fun file -> Filename.check_suffix file ".cmdi" || Filename.check_suffix file ".cmd") files in
  let global = create_global_from_files cmt_files in
  List.iter 
    (fun cmd -> let cmt = get_cmt cmd cmt_files in process_file global cmd cmt)
    cmd_files
    
