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


(** {2 Globals} *)
let index_content = ref []

let cpt = ref 0

let generate_unique_id () = 
  let name = "Anon_"^(string_of_int !cpt) in
  incr cpt;
  name

(** {2 Html generation} *)

(** {4 Values} *)

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

(** {4 Comments} *)

(* Do we print normal comments? *)
let html_of_comment env = function 
  | { si_item = `Comment; si_info = info} -> 
    (match info with 
      | Some i -> <:html<<br/>$i$<br/>
	  >>
      | None -> Html.nil)
  | _ -> assert false


(** {4 Types} *)

let html_of_type_variant father_name = function 
  | {tk_constructors = Some cstrs; _} ->
    make_type_table 
      (function {vc_name=name; vc_args=args; vc_info=info} ->
	let cell typ = <:html<<td align="left" valign="top"><code>$keyword "|"$</code></td><td align="left" valign="top"><code>$typ$</code></td>&>>
	in 
	let constr_name = 
	  generate_id_mark Opam_doc_config.mark_type_elt (father_name^"."^name)
	  (<:html<<span class="constructor">$str:name$</span>&>>)
	in 
	let constr = 
	  match args with
	    | [] -> cell constr_name
	    | l -> let l = insert_between " * " args
		   in cell (<:html<$constr_name$ $keyword "of"$ $code "type" l$>>)
	in let info = 
	     match info with 
	       | Some i -> 
		 make_field_comment i
	       | _ -> Html.nil
	   in
	   <:html<$constr$$info$
	   >>
      )
      cstrs
  | _ -> assert false

(* to improve :l *)
let html_of_type_record father_name = function
  | {tk_labels = Some lbls; _} ->
    make_type_table 
      (function {rl_name=name; rl_mut=mut; rl_typ=typ; rl_info=info} ->
	(* bug on &nbsp -> duck tape : simple space *)
	<:html<<td align="left" valign="top"><code>  </code></td><td align="left" valign="top"><code>$if mut then keyword "mutable" else Html.nil$ $generate_id_mark Opam_doc_config.mark_type_elt (father_name^"."^name) (html_of_string name)$ :$code "type" typ$;</code></td>$match info with Some i -> make_field_comment i | _ -> Html.nil$
	  &>>
      )
      lbls
  | _ -> assert false
    
let html_of_type env = function 
  | { si_item = `Type
    ; si_name = Some name
    ; si_params = Some params
    ; si_cstrs = cstrs (* option *)
    ; si_type_kind = Some type_kind
    ; si_priv = Some priv
    ; si_manifest = manifest (* option *)
    ; si_variance = Some variances
    ; si_info = info
    ;_} -> 
    let h_f = 
      match manifest, type_kind.tk_kind with
        | None, `Variant
        | None, `Record -> (fun x -> make_pre (code "" x))
        | _ -> make_pre
    in
    let name_html = 
      <:html<$keyword "type"$ $html_of_type_param_list params variances$$str:name$ >>
    in    
    let name_html = generate_id_mark Opam_doc_config.mark_type name name_html in
    let manifest = 
      match manifest, type_kind.tk_kind with 
	| Some typ,`Record -> <:html<= {$code "type" typ$ >>
        | Some typ,_ -> <:html<= $code "type" typ$ >>
	| None, `Record -> <:html<= {>>
        | None, `Abstract -> Html.nil
        | None, _ -> <:html<= >>
    in
    let signature =
      let priv = <:html<$if priv then keyword "private" else Html.nil$>>  in
      h_f <:html<$name_html$$manifest$$priv$>>
    in
    let html_typ = match type_kind.tk_kind with
	`Abstract -> signature
      | `Variant -> <:html<$signature$$html_of_type_variant name type_kind$>>
      | `Record -> <:html<$signature$$html_of_type_record name type_kind$}>>
    in
    <:html<$html_typ$
      $make_info info$
    >>
  | _ -> assert false


let rec html_of_class_type_ident = function
  | { ct_kind = `Ident; 
      ct_args = args;
      ct_params = Some params;
      ct_path = Some path; } ->
    let args = match args with None -> Html.nil | Some args ->
      code "type"
	(List.fold_left (fun acc typ -> <:html<$acc$$typ$ -> >>) Html.nil args)
    in 
    let params = html_of_type_class_param_list 
      params 
      (List.map (fun _ -> `None) params) in
    <:html<$args$$params$$path$>>
  | _ -> assert false
    
and html_of_class_type_sig = function
  | { ct_kind = `Sig; 
      ct_args = args (* option *);
      ct_fields = Some fields; } ->
    let args = match args with None -> Html.nil | Some args ->
      code "type"
	(List.fold_left (fun acc typ -> <:html<$acc$$typ$ -> >>) Html.nil args)
    in 
    let ref_link = <:html<..>> (*add link *) in
    <:html<$args$$code "code" (html_of_string "object")$ $ref_link$ $code "code" (html_of_string "end")$>>
  | _ -> assert false
    
and html_of_class_type_constraint = function
  | { ct_kind = `Constraint; 
    ct_args = args (* option *);
    ct_cstr = Some (ctype, cttype) } ->
    let args = match args with None -> Html.nil | Some args ->
      code "type"
	(List.fold_left (fun acc typ -> <:html<$acc$$typ$ -> >>) Html.nil args)
    in
    <:html<$args$( $html_of_class_type ctype$ : $html_of_class_type cttype$ )>>
  | _ -> assert false

(** Print the body of a class/class_type signature
    can maybe generate an url to the definition file
    look up on next step
*)
and html_of_class_type class_type =
  (match class_type.ct_kind with
    | `Ident -> html_of_class_type_ident 
    | `Sig -> html_of_class_type_sig
    | `Constraint -> html_of_class_type_constraint) class_type

(* url = CurrentModule.ClassName-c.html *)
let html_of_class env = function 
  | { si_item = typ
    ; si_name = Some name
    ; si_params = Some params
    ; si_variance = Some variances
    ; si_virt = Some virt
    ; si_class_type = Some class_type
    ; si_info = info; _ } -> 
    let id = generate_id_mark Opam_doc_config.mark_type name in
    let params_html = html_of_type_class_param_list params variances in
    let ref = <:html<$str:name$>> (* todo : referencing link *) in
    let header = 
      <:html<$keyword (if typ = `Class then  "class" else "class type")$$
	if virt then keyword " virtual" 
	else Html.nil$ $params_html$$ref$>> in
    let sign = if typ = `Class then "=" else ":" in
    let pre = make_pre <:html<$id header$ $str:sign$ $html_of_class_type class_type$>> in
    <:html<$pre$$make_info info$
    >>    
  | _ -> assert false 

(** {4 Exceptions} *)

let html_of_exception env = function 
  | { si_item = `Exception; si_name = Some name; 
      si_args = Some args; si_info = info} -> 
    let id = generate_id_mark Opam_doc_config.mark_exception name <:html<$keyword "exception"$ $str:name$>> 
    in let args = match args with 
      | [] -> Html.nil 
      | _ -> <:html< $keyword "of"$ $code "type" (insert_between " * " args)$>> in
    let signature = make_pre <:html<$id$$args$>> in
    <:html<$signature$$make_info info$>>
  | _ -> assert false

(** {4 Classes} *)



(** {4 Modules} *)

let rec  html_of_module_ident env = function
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
    let arg_signature = match arg_type.mt_path with 
      | Some p -> code "type" p
      | None -> 
	let anon_env = (add_to_env env (generate_unique_id ())) in
	(* anonymous module html creation *)
	generate_html ~env:anon_env (get_full_path_name anon_env) ~title_signature:Html.nil 
	  (Docjson.file (match arg_type.mt_items with Some its -> its | None -> []) None);
	(* signature definition *)
	html_of_module_body anon_env arg_type;
    in (* anonymous modules *)
    let body = 
      <:html<$code "code" (html_of_string "functor (")$$code "code" (html_of_string arg_name)$<code class="code"> : </code>$arg_signature$$code "code" (html_of_string ") -> ")$&>> in
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
	  <:html<$keyword "module"$ $reference$ : $body$>>
       else
	  <:html<$keyword "module type"$ $reference$ = $code "type" body$>>
      ) in
    
    (* recursive module generation *)
    
    (match base_mty with
      | {mt_kind=`Ident; mt_path=Some path; _} ->
	generate_html_symlink 
	  ~env:updated_env 
	  (get_full_path_name updated_env) 
	  signature
	  info
	  (Html_utils.extract_path path) (* can fail *)
      | {mt_kind=`Sig; mt_items = Some items; _} -> 
	generate_html 
	  ~env:updated_env 
	  (get_full_path_name updated_env) 
	  ~title_signature:signature
	  (Docjson.file items info)
      | _ -> assert false
    );
    let path = get_full_path_name updated_env 
      ^ ".html" 
    in
    let signature = <:html<$signature$$make_info info$>> in
    wrap_module signature path
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
	  "FIXME.html"
	   (* FIX ME *)
	  (*failwith "include anonymous module_type: Not implemented yet"*)
    in
    let open Html_utils in
	create_include_pages 
	  (get_full_path_name env)
	  path
	  (Index.lookup_include_module_type key);
	
	let signature = make_pre <:html<$keyword "include"$ $signature$>>
	in
	wrap_include signature path		  
      
  | _ -> assert false

and html_of_signature_item env item =
      let f = match item.si_item with 
	| `Module
	| `ModType -> html_of_module
	| `Include -> html_of_include 
	| `Value -> html_of_value
	| `Primitive -> (fun env item -> html_of_value env {item with si_item = `Value})
	| `Type -> html_of_type 
	| `Exception -> html_of_exception
	| `Class  
	| `ClassType -> html_of_class
	| `Comment -> html_of_comment
      in try f env item with Not_found -> <:html<FIX ME>>

(* module_name can also be a submodule
   eg: "M.Msub"
   in this case, it should be given the item signature
*)
and generate_html ?env module_name ?title_signature jfile =
  let c_env = match env with Some e -> e 
    | None -> add_to_index module_name jfile.f_info; 
    new_env module_name in
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
and generate_html_symlink ?env module_name title_signature info target_filename =
  let c_env = match env with 
    | Some e -> e 
    | None -> new_env module_name in
  let page_filename = get_full_path_name c_env ^ ".html" in
  Html_utils.(
    begin
      (*create_html_concrete_page page_filename title_signature module_name info;*)
      let target_name = Filename.basename (Filename.chop_extension target_filename ) in
      create_html_concrete_page_with_ref 
	page_filename module_name 
	title_signature info 
	target_name target_filename;

      create_html_symlink_contents page_filename target_filename
    end
  )

and add_to_index m_name info =
  let html_page = m_name^".html" in
  let content = 
    <:html<<tr><td class="module"><a href="$str:html_page$">$str:m_name$</a></td><td>
		     $make_first_line_info info$
			</td></tr>&>> in
  index_content := (m_name,content)::(!index_content)

let generate_module_index () = 
  match !index_content with
      [] -> ()
    | l -> 
      begin
	let full_content = 
	  List.fold_left
	    (fun acc (_,content) -> <:html<$acc$$content$>>) 
	    Html.nil
	    (List.sort (fun (n,_) (n2, _) -> String.compare n n2) l)
	in
	let oc = open_out (!Opam_doc_config.output_directory ^ "/index.html") in
	output_string oc Opam_doc_config.doctype;
	let html_content = 
	  <:html<
	  <html>
	  <head> 
	  <link rel="stylesheet" href="$str:Opam_doc_config.style_filename$" type="text/css" />
	  <title>Modules index</title>
	  </head>
	  <body>
	  <h1>Modules</h1>
	  <table class="indextable">
	     $full_content$
          </table>
	  </body>
	  </html>&>>
      in
      output_string oc (string_of_html html_content);
      close_out oc
      end

let output_style_file () =
  print_endline "Css generation";
  let oc = open_out (!Opam_doc_config.output_directory ^ Opam_doc_config.style_filename) in
  output_string oc (String.concat "\n" Opam_doc_config.default_stylesheet);
  close_out oc
    
let output_script_file () =
  print_endline "Script generation";
  let oc = open_out (!Opam_doc_config.output_directory ^ Opam_doc_config.script_filename) in
  output_string oc Opam_doc_config.default_script;
  close_out oc
    
(* Générer l'index global dans driver? *)
