open Docjson
open Cow

(** {2 Environnement handling}  *)

type doc_env = 
    {current_module_name:string;
     parent_modules:string list (** reversed list B.M.SubM -> ["M"; "B"] *) }
      
let new_env module_name = 
  {current_module_name=module_name; parent_modules=[]}

let add_to_env env module_name =
  {current_module_name=module_name; 
   parent_modules=env.current_module_name::env.parent_modules}

let get_full_path_name env =
  String.concat "." (List.rev (env.current_module_name::env.parent_modules))

(** {2 Html generation} *)

let rec to_be_removed () = () 

(* doc_env -> Docjson.module_type -> Html.t *)
and html_of_module_body env item = 
  Html.nil

(** Modules output *)
and html_of_module env =
  function {si_item=m_kind; 
	    si_name=Some name;
	    si_module_type = Some mty;
	    si_info = info; _} ->
    (* signature generation *)
    let base_mty = Html_utils.grab_base_module mty in (* ident | sig *)

    let signature = Html.nil in
    

    (* recursive module generation *)
    let updated_env = add_to_env env name in
    (match base_mty with
      | {mt_kind=`Ident; mt_path=Some path; _} ->
	generate_html_symlink 
	  ~env:updated_env 
	  (get_full_path_name updated_env) 
	  ~title_signature:signature
	  info
	  (Html_utils.extract_path path)
      | {mt_kind=`Sig; mt_items = Some items; _} -> 
	generate_html 
	  ~env:updated_env 
	  (get_full_path_name updated_env) 
	  ~title_signature:signature
	  (Docjson.file items info)
      | _ -> assert false
    );
    signature
    | _ -> assert false
  
and html_of_include env = function
  | {si_item=`Include; 
     si_module_type= Some mty;
     si_info = info; _ } as key -> 
    
    let signature = html_of_module_body env mty in

    (* Pages creations *)
    let path = 
      match Html_utils.grab_base_module mty with
	| {mt_kind=`Ident; mt_path=Some path; _} ->
	  Html_utils.extract_path path
	| _ -> (* how to get an anonymous module name? *)
	  failwith "include anonymous module_type: Not implemented yet"
    in
    let open Html_utils in
	create_include_pages 
	  (get_full_path_name env) 
	  path
	  (Index.lookup_include_module_type key);
	
	make_pre <:html<$keyword "include"$ $signature$>>
      
  | _ -> assert false

and html_of_signature_item env item =
  let f = match item.si_item with 
    | `Module
    | `ModType -> html_of_module
    | `Include -> html_of_include
    | `Value 
    | `Primitive -> fun _ _ -> <:html<a value !>>
    | `Type -> fun _ _ -> <:html<a type !>>
    | `Exception -> fun _ _ -> <:html<an exc !>>
    | `Class 
    | `ClassType -> fun _ _ -> <:html<a class !>>
    | `Comment -> fun _ _ -> <:html<a comment !>>
  in f env item

(* module_name can also be a submodule
   eg: "M.Msub"
   in this case, it should be given the item signature
*)
and generate_html ?env module_name ?title_signature jfile =
  let c_env = match env with Some e -> e | None -> new_env module_name in
  let page_filename = module_name ^ ".html" in
  let module_html_items =
    List.map (html_of_signature_item c_env) jfile.f_items
  in
  Html_utils.(
    begin
      create_html_concrete_page page_filename title_signature module_name jfile.f_info;
      create_html_page_contents page_filename module_html_items
    end
  )

(* target_filename should be : module_path.html *)
and generate_html_symlink ?env module_name ?title_signature info target_filename =
  let c_env = match env with Some e -> e | None -> new_env module_name in
  let page_filename = get_full_path_name c_env ^ ".html" in
  Html_utils.(
    begin
      create_html_concrete_page page_filename title_signature module_name info;
      create_html_symlink_contents page_filename target_filename
    end
  )


let () =
  print_endline "Generation du css";
  let oc = open_out Opam_doc_config.style_filename in
  output_string oc (String.concat "\n" Opam_doc_config.default_stylesheet);
  close_out oc
    
let () =
  print_endline "Generation du script";
  let oc = open_out Opam_doc_config.script_filename in
  output_string oc Opam_doc_config.default_script;
  close_out oc
    
(* Générer l'index global dans driver? *)
