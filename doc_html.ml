open Docjson
open Cow
open Html_utils

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

(** {4 Values} *)

let html_of_value = function 
  | {si_item = `Value;
     si_name = Some name;
     si_typ = Some typ;
     si_info = info; _ }
  | {si_item = `Primitive;
     si_name = Some name;
     si_typ = Some typ;
     si_info = info; _ } ->
    let name_val = generate_mark 
      Opam_doc_config.mark_value name <:html<$keyword "val"$ $str:name$>> in
    let node_val = <:html<$name_val$ : $code "type" typ$>>
    in
    <:html< 
      $make_pre node_val$
      $make_info info$
    >>
    | _ -> assert false

(** {4 Comments} *)

(* Do we print normal comments? *)
let html_of_comment = function 
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
	  generate_mark Opam_doc_config.mark_type_elt (father_name^"."^name)
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
	<:html<<td align="left" valign="top"><code>  </code></td><td align="left" valign="top"><code>$if mut then keyword "mutable" else Html.nil$ $generate_mark Opam_doc_config.mark_type_elt (father_name^"."^name) (html_of_string name)$ : $code "type" typ$;</code></td>$match info with Some i -> make_field_comment i | _ -> Html.nil$
        &>>
      )
      lbls
  | _ -> assert false
    
let html_of_type = function 
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
      <:html<$keyword "type"$ $html_of_type_param_list params variances$$str:name$>>
    in    
    let name_html = generate_mark Opam_doc_config.mark_type name name_html in
    let manifest = 
      match manifest, type_kind.tk_kind with 
	| Some typ,`Record -> <:html<= {$code "type" typ$}>>
        | Some typ,_ -> <:html<= $code "type" typ$ >>
	| None, `Record -> <:html<= {>>
        | None, `Abstract -> Html.nil
        | None, _ -> <:html<= >>
    in
    let signature =
      let priv = <:html<$if priv then keyword "private" else Html.nil$>>  in
      h_f <:html<$name_html$ $manifest$$priv$>>
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


(** {4 Classes} *)

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

and html_of_inherit = function
  | { ctf_field = `Inherit
    ; ctf_class_type = Some cty
    ; ctf_info = info
    ;  _ } -> 
    let signature = make_pre 
      <:html<$keyword "inherit"$ $html_of_class_type cty$$make_info info$>> 
    in
    (* generating meta-data tags *)
    wrap_class "inherit_field" signature cty
  | _ -> assert false

and html_of_class_field = function
  | { ctf_field = `Inherit; _ } as field -> html_of_inherit field
  | { ctf_field = `Val
    ; ctf_name = Some name
    ; ctf_mut = Some mut
    ; ctf_virt = Some virt
    ; ctf_typ = Some typ
    ; ctf_info = info
    ;  _ } -> 
    let label = keyword "val" in
    let label = if virt then <:html<$label$ $keyword "virtual"$>> else label in
    let label = if mut then <:html<$label$ $keyword "mutable"$>> else label in
    let label = generate_mark Opam_doc_config.mark_attribute name 
      <:html<$label$ $str:name$>> in
    let signature = make_pre <:html<$label$ : $code "code" typ$>> in
    <:html<$signature$$make_info info$>>
  |  { ctf_field = `Method
     ; ctf_name = Some name
     ; ctf_virt = Some virt
     ; ctf_priv = Some priv
     ; ctf_typ = Some typ
     ; ctf_info = info
     ; _} ->  
    let label = keyword "method" in
    let label = if priv then <:html<$label$ $keyword "private"$>> else label in
    let label = if virt then <:html<$label$ $keyword "virtual"$>> else label in
    let label = generate_mark Opam_doc_config.mark_method 
      name <:html<$label$ $str:name$>> in
    let signature = make_pre <:html<$label$ : $code "code" typ$>> in
    <:html<$signature$$make_info info$>>
  | { ctf_field = `Constraint
    ; ctf_eq = Some (typ1, typ2)
    ; ctf_info = info
    ; _  } -> (* TODO handle class constraints *) Html.nil

  | { ctf_field = `Comment
    ; ctf_info = info
    ; _ } -> make_info info
    
  | _ -> Html.nil (* assert false *)

and html_of_class_type_sig = function
  | { ct_kind = `Sig
    ; ct_args = args (* option *)
    ; ct_fields = Some fields
    ; _ } ->
    let args = match args with None -> Html.nil | Some args ->
      code "type"
	(List.fold_left (fun acc typ -> <:html<$acc$$typ$ -> >>) Html.nil args)
    in 
    <:html<$args$$code "code" (html_of_string "object")$ .. $code "code" (html_of_string "end")$>>
  | _ -> assert false
    
and html_of_class_type_constraint = function
  | { ct_kind = `Constraint
    ; ct_args = args (* option *)
    ; ct_cstr = Some (ctype, cttype)
    ; _ } ->
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

and wrap_class name html_content = function
	| {ct_kind=`Ident; 
	   ct_path=Some path;
	   (* ct_params=Some params (* do something there *)*)
	   _} -> 
	  let path =
	    try Some (extract_path path) with Not_found -> None
	  in
	  wrap_ident_class html_content name path	   
	| {ct_kind = `Sig; 
	   ct_fields= Some fields;
	   _ } -> 
	  let sub_elements = List.map html_of_class_field fields in
	  wrap_sig_class 
	    html_content
	    (fold_html sub_elements)
	    name
	| {ct_kind=`Constraint;
	   ct_cstr=Some (ctyp, _);
	   _ } -> wrap_class name html_content ctyp
	| _ -> assert false

let html_of_class env = function 
  | { si_item = typ
    ; si_name = Some name
    ; si_params = Some params
    ; si_variance = Some variances
    ; si_virt = Some virt
    ; si_class_type = Some class_type
    ; si_info = info; _ } -> 
    let id = generate_mark Opam_doc_config.mark_type name in
    let params_html = html_of_type_class_param_list params variances in
    let query_string = 
      "?package=" ^ !Opam_doc_config.current_package
      ^"&module="^get_full_path_name env
      ^"&class="^name in
    let reference = <:html<<a href="$uri:Uri.of_string query_string$">$str:name$</a>&>> in
    let header = 
      <:html<$keyword (if typ = `Class then  "class" else "class type")$$
	if virt then keyword " virtual" 
	else Html.nil$ $params_html$$reference$>> in
    let sign = if typ = `Class then "=" else ":" in
    let signature = make_pre <:html<$id header$ $str:sign$ $html_of_class_type class_type$>> in
    let html_content = <:html<$signature$$make_info info$>> in
    
    (* generating meta-data tags *)
    wrap_class name html_content class_type

  | _ -> assert false 

(** {4 Exceptions} *)

let html_of_exception = function 
  | { si_item = `Exception; si_name = Some name; 
      si_args = Some args; si_info = info} -> 
    let id = generate_mark Opam_doc_config.mark_exception name <:html<$keyword "exception"$ $str:name$>> 
    in let args = match args with 
      | [] -> Html.nil 
      | _ -> <:html< $keyword "of"$ $code "type" (insert_between " * " args)$>> in
    let signature = make_pre <:html<$id$$args$>> in
    <:html<$signature$$make_info info$>>
  | _ -> assert false
    

(** {4 Modules} *)

let rec  html_of_module_ident = function
  | { mt_kind = `Ident; mt_path = Some path; _} -> 
    <:html<$code "code" path$&>>
  | _ -> <:html<HTML_OF_MODULE_IDENT BUG>>
    (* assert false *)

(** Generate the submodule file at this point *)
and html_of_module_sig = function
  | { mt_kind = `Sig; (*mt_items = Some items;*) _ } ->
    <:html<$code "code" (html_of_string "sig")$ .. $code "code" (html_of_string "end")$>>
  | _ -> assert false

and html_of_module_functor = function 
  | { mt_kind = `Functor
    ; mt_arg_name = Some arg_name
    ; mt_arg_type = Some arg_type
    ; mt_base = Some base; _} ->
    let arg_signature = match arg_type.mt_path with 
      | Some p -> code "type" p
      | None -> html_of_module_body arg_type (* anonymous modules *)
    in 
    let body = 
      <:html<$code "code" (html_of_string "functor (")$$code "code" (html_of_string arg_name)$<code class="code"> : </code>$arg_signature$$code "code" (html_of_string ") -> ")$&>> in
    <:html<<div class="sig_block">$body$$html_of_module_body base$</div>&>>
  | _ -> raise (Failure "html_of_functor: Mismatch")
    
and html_of_module_with = function
  | { mt_kind = `With; 
      mt_cnstrs = Some cnstrs; 
      mt_base = Some base; _ } ->
    let l = List.map 
      (function 
	| {wc_typeq = Some typ; wc_path = path; 
	   wc_subst = subst; _} -> 
	  let sgn = if subst then ":=" else "=" in
	  <:html<type $path$ $str:sgn$ $typ$>>
        | {wc_path = path; wc_modeq = Some modeq;
	   wc_subst = subst; _} -> 
	  let sgn = if subst then ":=" else "=" in
	  <:html<module $path$ $str:sgn$ $modeq$>>
	| _ -> assert false)
      cnstrs in
    <:html<$html_of_module_body base$ with $insert_between " and " l$>>
  | _ -> assert false

and html_of_module_typeof  = function 
  | { mt_kind = `TypeOf; mt_base = Some base; _} ->
    let base_html = html_of_module_body base in
    <:html<module type of $base_html$>>
  | _ -> assert false

and html_of_module_apply = function
  | { mt_kind = `Apply; 
      mt_arg_type = Some arg_type; 
      mt_base = Some base; _ } -> 
    let base_html = html_of_module_ident base in
    let arg_html = html_of_module_body arg_type in
    <:html<$base_html$($arg_html$)>>
  | _ -> assert false

and html_of_module_body mty =
  (match mty.mt_kind with 
  | `Ident   -> html_of_module_ident  
  | `Sig     -> html_of_module_sig
  | `Functor -> html_of_module_functor 
  | `With    -> html_of_module_with   
  | `Apply   -> html_of_module_apply
  | `TypeOf  -> html_of_module_typeof) mty

(** Modules output *)
and html_of_module env =
  function {si_item=m_kind; 
	    si_name=Some name;
	    si_module_type = Some mty;
	    si_info = info; _} ->
    let body = html_of_module_body mty in
    let new_env = add_to_env env name in
    let query_string = 
      "?package=" ^ !Opam_doc_config.current_package
      ^"&module=" ^ get_full_path_name new_env in
    let reference = <:html<<a href="$uri:Uri.of_string query_string$">$str:name$</a>&>> in
    let signature = make_pre
      (if m_kind = `Module then
	  <:html<$keyword "module"$ $reference$ : $code "type" body$>>
       else
	  <:html<$keyword "module type"$ $reference$ = $code "type" body$>>
      ) in
    let constraints = match extract_constraints mty with
      | [] -> Html.nil
      | l -> <:html<<div style="display:none" class="constraints">$fold_html l$</div>&>>  
    in
    let html_content = <:html<$constraints$$signature$$make_info info$>> in
    
    (* generating meta-data tags *)
    begin
      match (Html_utils.grab_base_module mty) with
	| {mt_kind = `Ident; mt_path=Some path; _} -> 
	  let reference = 
	    try Some (extract_path path) with Not_found -> None
	  in
	  wrap_ident_module html_content name reference	   
	| {mt_kind = `Sig; mt_items = Some items; _ } -> 
	  let sub_elements = 
	    List.map (html_of_signature_item (add_to_env env name)) items in
	  wrap_sig_module
	    html_content 
	    (fold_html sub_elements) (* elements *)
	    name
	| _ -> (* TODO *) assert false
    end
  | _ -> assert false
  
and html_of_include env = function
  | {si_item=`Include; 
     si_module_type= Some mty;
     si_info = info; _ } as key -> 
    
    let module_sig = code "type" (html_of_module_body mty) in
    let signature = make_pre <:html<$keyword "include"$ $module_sig$$make_info info$>> in
    let path,elems =
      match (Html_utils.grab_base_module mty) with
	| {mt_kind = `Ident; mt_path=Some path; _} -> 
	  begin
	    try Some (extract_path path), None
	    with Not_found -> None, None
	  end
	| {mt_kind = `Sig; mt_items = Some items; _ } -> 
	  let sub_elements = 
	    List.map (html_of_signature_item env) items in
	  None, Some (fold_html sub_elements)
	| _ -> (* TODO *) assert false
    in
    wrap_include key path elems signature
  | _ -> assert false
    
and html_of_signature_item env item =
      let rec f = match item.si_item with 
	| `Module
	| `ModType -> html_of_module env
	| `Include -> html_of_include env
	| `Value 
	| `Primitive -> html_of_value
	| `Type -> html_of_type 
	| `Exception -> html_of_exception
	| `Class  
	| `ClassType -> html_of_class env
	| `Comment -> html_of_comment
      in f item

let generate_html module_name jfile =
  let filename = !Opam_doc_config.current_package ^ "/" ^  module_name ^ ".html" in
  let elements = List.map 
    (html_of_signature_item (new_env module_name))
    jfile.f_items in  
  let oc = open_out filename in
  output_string oc "<div class=\"ocaml_toplevel_module\">";
  output_string oc (string_of_html (make_info jfile.f_info));
  List.iter (fun e -> output_string oc (string_of_html e)) elements;
  output_string oc "</div>";
  close_out oc;
  Some (module_name, make_first_line_info jfile.f_info)


let output_style_file () =
  print_endline "Css generation";
  let oc = open_out Opam_doc_config.style_filename in
  output_string oc (String.concat "\n" Opam_doc_config.default_stylesheet);
  close_out oc
    
let output_script_file () =
  print_endline "Script generation";
  let oc = open_out Opam_doc_config.script_filename in
  output_string oc Opam_doc_config.default_script;
  close_out oc
   
(* let create_default_page global filename = 
  Html_utils.create_html_default_page global
    filename "OpamDoc - Ocaml Documentation"      
*)
let generate_module_index = function
  | [] -> ()
  | l ->
    let make_content (m_name, info) = 
      let uri = Uri.of_string 
	("?package="^ !Opam_doc_config.current_package
	^"&module="^m_name) in
      <:html<<tr><td class="module"><a href="$uri:uri$">$str:m_name$</a></td><td>$info$</td></tr>
>> 
    in
    let oc = open_out (!Opam_doc_config.current_package ^ "/index.html") in
    let html_content =
      <:html<<h1>Modules</h1>
<table class="indextable">
    $fold_html (List.map make_content l)$
</table>&>>
    in
    output_string oc (string_of_html html_content);
    close_out oc
