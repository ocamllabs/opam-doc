type t_value = 
  | Direct_path of string
  | Packed_module of (string * Digest.t) list (* sub modules ? *)

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

module StringMap = Map.Make(String)

type global = t_value CrcMap.t

type local = t_value StringMap.t

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

let local_print table =
  StringMap.iter
    (fun x z -> 
      Printf.printf "key : %s - value: %s\n" x
	(match z with
	    Packed_module crc -> 
	      "Packed\n"
	      ^(List.fold_left (fun acc (x,y) -> acc^"\n\t"^x^" - "^(Digest.to_hex y)) ""
		  crc)
	  | Direct_path str -> "Direct path : "^str))
    table

let global_lookup global md = CrcMap.find md global

let rec create_local global mds = 
  let doMod acc ((name, _) as md) =
    try
      let value = CrcMap.find md global in
      match value with
	| Packed_module crcs -> 
	  StringMap.add name value (create_local global crcs)
	| Direct_path str ->
	  (* Is it true that names are unique in the local index? *)
	  StringMap.add name value acc
    with Not_found -> acc
  in
  List.fold_left doMod StringMap.empty mds
    
let local_lookup local md = 
  match StringMap.find md local with
    | Packed_module _ -> None
    | Direct_path str -> Some str
      

let get_global_modules global =
  List.map (fun ((x,_),_) -> x) (CrcMap.bindings global)  
