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

type package_list = (string * Cow.Html.t option) list

type global = { map: t_value CrcMap.t
	      ; package_list: package_list }

type local = t_value LocalMap.t

let read_global_file path =
  if Opam_doc_config.clear_index () then
    {map= CrcMap.empty; package_list= []}
  else
    try
      let ic = open_in path in
      let global = input_value ic in
      close_in ic;
      global
    with
      | Sys_error _ -> {map= CrcMap.empty; package_list= []}

let update_global global filenames =
  let doFile acc fname =
    let cmio, cmto = Cmt_format.read fname in
    match cmio, cmto with
      (* looking up for a packed cmt *)
      | Some cmi, Some cmt ->
	let module_name = cmi.Cmi_format.cmi_name in
	let (name,crc) =
	  match cmi.Cmi_format.cmi_crcs with
	    | e::_ ->
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
		   (Opam_doc_config.current_package (),
		    (List.filter ((<>) (name,crc)) cmi.Cmi_format.cmi_crcs)))
		acc
	    | Cmt_format.Implementation _
	    | Cmt_format.Interface _ ->
	      CrcMap.add (module_name, crc)
		(Direct_path (Opam_doc_config.current_package (), module_name))
		acc
	    | _ -> acc (* shouldn't happen but you never know :l *)
	end
      | _ ->
	Printf.eprintf "Index.update_global: %s -- Wrong cmt file handed\n%!" fname;
	acc
  in
  let newmap = List.fold_left doFile global.map filenames in
  { global with map = newmap }

let write_global_file global path =
  let oc = open_out path in
  output_value oc global;
  close_out oc

let create_local global mds =
  let rec doMod pack acc ((name, _) as md)  =
    try
      let value = CrcMap.find md global.map in
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
let rec assemble_path is_class (pack, path) = function
  | [] -> "?package="^pack^"&module="^path
  | h::[] ->
    if is_module h then
      "?package="^pack^"&module="^path^"."^h
    else
      "?package="^pack^"&module="^path^(if is_class then "&class=" else "&type=")^h
  | h::t ->
    if not (is_module h) then raise (Failure "Incorrect signature");
    assemble_path is_class (pack, path^"."^h) t

let local_lookup local ?(is_class=false) path_elems =
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
  assemble_path is_class (package,path) rest

let get_global_packages global =
  global.package_list

let package_exists global package_name =
  List.exists (fun (n,_) -> n = package_name) global.package_list

let add_global_package global package_name info =
  let info_opt = if info = "" then None else Some (Cow.Html.of_string info) in
  let rec add_or_replace acc = function
    | [] -> List.rev ((package_name, info_opt)::acc)
    | (h,_)::t when h = package_name ->
      (package_name, info_opt)::acc@t
    | h::t -> add_or_replace (h::acc) t
  in
  {global with package_list = (add_or_replace [] global.package_list)}

(* Internal references part *)
let internal_table = Hashtbl.create 32

let reset_internal_reference_table () =
  Hashtbl.reset internal_table

let add_internal_reference = Hashtbl.add internal_table

let lookup_internal_reference = Hashtbl.find internal_table
