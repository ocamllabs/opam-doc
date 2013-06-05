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
(*
let process_file global cmd cmt = 
  let doc_tree = process_cmd cmd in
  let cmi, cmt = Cmt_format.read cmt in
  match cmi, cmt with
    | _, None -> raise (Failure "Not a cmt file")
    | None, Some cmt -> raise (Failure "I need the cmti")
    | Some cmi, Some cmt ->
      let imports = cmi.Cmi_format.cmi_crcs in
      let local = create_local global imports in
      match cmt.Cmt_format.cmt_annots with
          Cmt_format.Interface intf -> process_interface local intf
        | Cmt_format.Implementation impl -> process_implementation impl print_endline "IMPLEM"
        | _ -> raise (Failure "Wrong kind of cmt file")
*)      
let process_file global cmd cmt = 
  let doc_file = match process_cmd cmd with
      Doctree.Dfile_intf intf -> intf | _ -> raise (Failure "") in
  let intf, imports = 
    let cmi, cmt = Cmt_format.read cmt in
    match cmi, cmt with
      | _, None -> raise (Failure "Not a cmt file")
      | None, Some cmt -> raise (Failure "Not implemented: .cmt files")
      | Some cmi, Some cmt -> 
          match cmt.Cmt_format.cmt_annots with
            Cmt_format.Interface intf -> intf, cmi.Cmi_format.cmi_crcs
          | Cmt_format.Implementation _ -> raise (Failure "Not implemented: .cmt files")
          | _ -> raise (Failure "Wrong kind of cmt file")
  in
  let local = create_local global imports in
  let jintf = generate_file local doc_file intf in
  let json = Docjson.json_of_file jintf in
  let json_name = (Filename.chop_extension cmd) ^ ".json" in
  let oc = open_out json_name in
  output_string oc (json_to_string json);
  close_out oc
  (* mon truc *)
  ; 
  Doc_html.(
    let module_name = String.capitalize 
      (Filename.chop_extension (Filename.basename cmd)) in
    let html = html_of_file module_name jintf in
    let html_name = module_name ^ ".html" in
    let oc = open_out html_name in
    output_string oc doctype;
    output_string oc (Docjson.string_of_html html);
    close_out oc;
  )

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
    
