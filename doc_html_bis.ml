open Docjson
open Cow
open Html_utils
open TagsGenerators

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

let html_of_value env = function 
  | {si_item = `Value;
     si_name = Some name;
     si_typ = Some typ;
     si_info = info; _ } ->  
    let node_val = 
      <:html<$keyword "val"$ $str:name$ : $code "type" typ$>>
    in
    <:html< 
      $make_pre (make_span node_val)$
      $make_info info$
    >>
    | _ -> assert false


(* Do we print normal comments? *)
let html_of_comment env = function 
  | { si_item = `Comment; si_info = info} -> 
    (match info with 
      | Some i -> <:html<<br/>$i$<br/>
	  >>
      | None -> Html.nil)
  | _ -> assert false


let rec to_be_removed () = () 

and html_of_module_ident env = function
  | { mt_kind = `Ident; mt_path = Some path} -> 
    <:html<$code "code" path$&>>
  | _ -> <:html<HTML_OF_MODULE_IDENT BUG>>
    (* assert false *)

(** Generate the submodule file at this point *)
and html_of_module_sig env = function
  | { mt_kind = `Sig; mt_items = Some items } ->
    let path = get_full_path_name env^".html" in
    <:html<$code "code" (html_of_string "sig")$ <a href="$str:path$">..</a> $code "code" (html_of_string "end")$>>
  | _ -> assert false

and html_of_module_functor env = function 
  | { mt_kind = `Functor
    ; mt_arg_name = Some arg_name
    ; mt_arg_type = Some arg_type
    ; mt_base = Some base} ->
    let path = match arg_type.mt_path with 
      | Some p -> code "type" p
      | None -> Html.nil in (* find a nice way of including the structural modules *)
    let body = 
      <:html<$code "code" (html_of_string "functor (")$$code "code" (html_of_string arg_name)$<code class="code"> : </code>$path$$code "code" (html_of_string ") -> ")$&>> in
    <:html<<div class="sig_block">$body$$html_of_module_body env base$</div>&>>
  | _ -> raise (Failure "html_of_functor: Mismatch")
    
and html_of_module_with env = function
  | { mt_kind = `With; 
      mt_cnstrs = Some cnstrs; 
      mt_base = Some base } ->
    let l = List.map 
      (function 
	| {wc_typeq = Some typ; wc_path = path} ->  <:html<type $path$ = $typ$>> 
        | {wc_path = path; wc_modeq = Some modeq} -> <:html<module $path$ = $modeq$>>
	| _ -> assert false)
      cnstrs in
    <:html<$html_of_module_body env base$ with $insert_between " and " l$>>
  | _ -> assert false
    

and html_of_module_typeof env = function 
  | { mt_kind = `TypeOf; mt_expr = Some ({me_path=Some p; _})} ->
    <:html<module type of $p$>>
  | _ -> assert false

and html_of_module_apply env = function
  | { mt_kind = `Apply; 
      mt_arg_type = Some arg_type; 
      mt_base = Some base } -> 
    let base_html = html_of_module_ident env base in
    let arg_html = html_of_module_body env arg_type in
    <:html<$base_html$($arg_html$)>>    
  | _ -> assert false

and html_of_module_body env mty =
  (match mty.mt_kind with 
  | `Ident   -> html_of_module_ident  
  | `Sig     -> html_of_module_sig
  | `Functor -> html_of_module_functor 
  | `With    -> html_of_module_with   
  | `Apply   -> html_of_module_apply
  | `TypeOf  -> html_of_module_typeof) env mty

(** Modules output *)
and html_of_module env =
  function {si_item=m_kind; 
	    si_name=Some name;
	    si_module_type = Some mty;
	    si_info = info; _} ->
    let updated_env = add_to_env env name in
    let base_mty = Html_utils.grab_base_module mty in (* ident | sig *)
    
    (* signature generation *)
    let reference = make_reference name (get_full_path_name updated_env ^ ".html") in
    let body = html_of_module_body updated_env mty in
    let signature = make_pre
      (if m_kind = `Module then
	  <:html<$keyword "module"$ $reference$ : $body$$make_info info$>>
       else
	  <:html<$keyword "module type"$ $reference$ = $code "type" body$$make_info info$>>
      ) in
    
    (* recursive module generation *)
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
	
	let signature = make_pre <:html<$keyword "include"$ $signature$>>
	in
	<:html<<div style="border:1px solid black">$signature$
	  $create_content_to_load_tag (path^Opam_doc_config.page_contents_extension)$
	</div>&>>
		  
      
  | _ -> assert false

and html_of_signature_item env item =
      let f = match item.si_item with 
	| `Module
	| `ModType -> html_of_module
	| `Include -> html_of_include
	| `Value -> html_of_value
	| `Primitive -> (fun env item -> html_of_value env {item with si_item = `Value})
	| `Type -> fun _ _ -> <:html<todo : types>> (*html_of_type*)
	| `Exception -> fun _ _ -> <:html<todo exception>> (*html_of_exception*)
	| `Class  
	| `ClassType -> fun _ _ -> <:html<todo classes>> (*html_of_class*)
	| `Comment -> html_of_comment
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
  print_endline "Css generation";
  let oc = open_out Opam_doc_config.style_filename in
  output_string oc (String.concat "\n" Opam_doc_config.default_stylesheet);
  close_out oc
    
let () =
  print_endline "Script generation";
  let oc = open_out Opam_doc_config.script_filename in
  output_string oc Opam_doc_config.default_script;
  close_out oc
    
(* Générer l'index global dans driver? *)
