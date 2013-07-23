(* | Direct_path : package * module 
   | Packed_module : package * imports
*)

type t_value = 
  | Direct_path of string * string
  | Packed_module of string * (string * Digest.t) list

module CrcMap = 
  Map.Make(struct 
    type t = string * Digest.t
    let compare (s1, d1) (s2, d2) = 
      let sc = String.compare s1 s2 in
      if sc = 0 then 
        Digest.compare d1 d2
      else sc
  end)

module LocalMap = Map.Make(
  struct
    type t = string option * string (* Packer * name *)
    let compare (s1, d1) (s2,d2) =
      let sc = compare s1 s2 in
      if sc = 0 then
	compare d1 d2
      else sc
  end)

type global = t_value CrcMap.t

type local = t_value LocalMap.t

let read_global_file path =
  try 
    let ic = open_in path in
    let global = input_value ic in
    close_in ic;
    global 
  with
    | Sys_error _ -> CrcMap.empty

let update_global global filenames =
  let doFile acc fname =
    let cmio, cmto = Cmt_format.read fname in
    match cmio, cmto with
      (* looking up for a packed cmt *)
      | Some cmi, Some cmt -> 
	let module_name = cmi.Cmi_format.cmi_name in
	let (name,crc) = 
	  match cmi.Cmi_format.cmi_crcs with
	    | e::l -> 
	      e
	    | _ -> assert false
	in
	begin 
	  match cmt.Cmt_format.cmt_annots with 
	    (* If this is a packed module, we need a way to find the path to submodules *)
	    | Cmt_format.Packed _ -> 
	      (* The references should be present in the table after the update *)      
	      CrcMap.add
		(module_name, crc)
		(* Filter the self-references *)
		(Packed_module 
		    (!Opam_doc_config.current_package,
		     (List.filter ((<>) (name,crc)) cmi.Cmi_format.cmi_crcs)))
		acc
	    | Cmt_format.Implementation _
	    | Cmt_format.Interface _ ->
	      CrcMap.add (module_name, crc)
		(Direct_path (!Opam_doc_config.current_package, module_name))
		acc
	    | _ -> acc (* shouldn't happen but you never know :l *)
	end
      | _ ->
	(* acc *)
	print_endline fname;
	raise (Failure "Index.update_global: Wrong cmt file handed")
  in
  List.fold_left doFile global filenames

    
let write_global_file global path =
  let oc = open_out path in
  output_value oc global;
  close_out oc
    
(* debug *)
let global_print table =
  CrcMap.iter
    (fun (x,y) z -> 
      Printf.printf "key : (%s,%s) - value: %s\n" x (Digest.to_hex y)
	(match z with
	    Packed_module _ -> "Packed"
	  | Direct_path (package, str) -> "Direct path - package : "
	    ^package^" module : "^str))
    table

let global_find_key (global:global) key =
  CrcMap.iter
    (fun (x,y) z ->
      if x = key then
	Printf.printf "key : (%s,%s) - value: %s\n" x (Digest.to_hex y) 
	  (match z with
	      Packed_module (_, sub_modules) -> "Packed\n"^
		(List.fold_left 
		   (fun acc (x,y) -> 
		     acc^"\t Modname:"^x^" - Digest : "
		     ^(Digest.to_hex y)^"\n") 
		   ""
		   sub_modules)
	    | Direct_path (_, str) -> "Direct path : "^str))
    global

let local_print table = ()

let global_lookup global md = CrcMap.find md global

let create_local global mds = 
  let rec doMod pack acc ((name, _) as md)  =
    try
      let value = CrcMap.find md global in
      match value with
	| Packed_module (_,crcs) -> 
	  let acc = List.fold_left (doMod (Some name)) acc crcs in
	  LocalMap.add (pack, name) value acc
	| Direct_path _ ->
	  LocalMap.add (pack, name) value acc
    with Not_found -> acc
  in
  List.fold_left (doMod None) LocalMap.empty mds

let is_module name =
  name.[0] >= 'A' && name.[0] <= 'Z'

(* assuming that if a type name is present, it is at the end of the list *)
let rec assemble_path (pack, path) = function 
  | [] -> "?package="^pack^"&module="^path
  | h::[] ->
    if is_module h then
      "?package="^pack^"&module="^path^"."^h
    else 
      "?package="^pack^"&module="^path^"&type="^h
  | h::t ->
    if not (is_module h) then raise (Failure "Incorrect signature");
    assemble_path (pack, path^"."^h) t

let local_lookup local path_elems =
  let rec loop pack = function
    | m1::m2::r ->
      begin
	match LocalMap.find (pack, m1) local with
	  | Direct_path (pack, str) -> (pack,str), (m2::r)
	  | Packed_module _ -> 
	    loop (Some m1) (m2::r)
      end
    | m1::[] ->
      begin
      match LocalMap.find (pack, m1) local with
	| Packed_module _ -> raise Not_found
	| Direct_path (pack, str) -> (pack, str), []
      end
    | [] -> raise Not_found    
  in
  let ((package,path), rest) = loop None path_elems in
  assemble_path (package,path) rest

let get_global_packages global =
  let open Hashtbl in
      let t = create 32 in
      CrcMap.iter 
	(fun _ -> function
	  | Direct_path (package, _) | Packed_module (package, _) ->
	    if not (mem t package) then
	      add t package 0
	)
	global;
      fold (fun k _ acc -> k::acc) t []	    
      

(* Internal references part *)
open Hashtbl 

let internal_table = create 32

let current_module = ref ""

let reset_internal_references module_name = 
  current_module := module_name;
  reset internal_table
    
let add_internal_reference = add internal_table
   
let lookup_internal_reference id = !current_module::(find internal_table id)  

(* Includes internal types *)

let include_table = create 4

let reset_include_table () = reset include_table
  
let add_include_module_type = add include_table

let lookup_include_module_type = find include_table 
