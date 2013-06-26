type t_value = 
  | Direct_path of string
  | Packed_module of (string * Digest.t) list

(** 
    key : (Module name, Digest)
    value : cmt path -> p-e mettre le path Html
*)
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
  (* TODO : get a better way of finding the path *)
  let current_path = (Sys.getcwd ())^"/" in
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
		(Packed_module (List.filter ((<>) (name,crc)) cmi.Cmi_format.cmi_crcs))
		acc
	    | Cmt_format.Implementation _
	    | Cmt_format.Interface _ ->
	      CrcMap.add (module_name, crc)
		(Direct_path (current_path^module_name^".html")) 
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
	  | Direct_path str -> "Direct path : "^str))
    table

let global_find_key (global:global) key =
  CrcMap.iter
    (fun (x,y) z ->
      if x = key then
	Printf.printf "key : (%s,%s) - value: %s\n" x (Digest.to_hex y) 
	  (match z with
	      Packed_module sub_modules -> "Packed\n"^
		(List.fold_left 
		   (fun acc (x,y) -> 
		     acc^"\t Modname:"^x^" - Digest : "
		     ^(Digest.to_hex y)^"\n") 
		   ""
		   sub_modules)
	    | Direct_path str -> "Direct path : "^str))
    global

let local_print table = ()

let global_lookup global md = CrcMap.find md global

let create_local global mds = 
  let rec doMod pack acc ((name, _) as md)  =
    try
      let value = CrcMap.find md global in
      match value with
	| Packed_module crcs -> 
	  let acc = List.fold_left (doMod (Some name)) acc crcs in
	  LocalMap.add (pack, name) value acc
	| Direct_path str ->
	  LocalMap.add (pack, name) value acc
    with Not_found -> acc
  in
  List.fold_left (doMod None) LocalMap.empty mds

let is_module name =
  name.[0] >= 'A' && name.[0] <= 'Z'

(* assuming that if a type name is present, it is at the list's end *)
let rec assemble_path path = function 
  | [] -> path^".html"
  | h::[] ->
    if is_module h then
      path^"."^h^".html"
    else 
      path^".html#TYPE"^h
  | h::t ->
    if not (is_module h) then raise (Failure "Incorrect signature");
    assemble_path (path^"."^h) t


let local_lookup local path_elems =
  let rec loop pack = function
    | m1::m2::r ->
      let f () = 
	try 
	  (match LocalMap.find (pack, m1) local with
	    | Direct_path str -> str, (m2::r)
	    | Packed_module _ -> 
	      loop (Some m1) (m2::r)
	  )
	with 
	  | Not_found -> 
		   (* try harder *)
	    loop pack (m2::r)
      in
      if m1 = "Std" then
	try 
	  loop pack (m2::r)
	with
	    Not_found -> f ()
      else
	f ()

    | m1::[] ->
      (match LocalMap.find (pack, m1) local with
	| Packed_module _ -> raise Not_found
	| Direct_path str -> str, []
      )
    | [] -> raise Not_found    
  in
  let (path, rest) = loop None path_elems in
  assemble_path (Filename.chop_suffix path ".html") rest

let get_global_modules global =
  List.map (fun ((x,_),_) -> x) (CrcMap.bindings global)  

(* Internal references part *)
open Hashtbl 

let internal_table = create 16

let current_module = ref ""

let reset_internal_references module_name = 
  current_module := module_name;
  reset internal_table
    
let add_internal_reference = add internal_table
   
let lookup_internal_reference id = !current_module::(find internal_table id)
  
    
