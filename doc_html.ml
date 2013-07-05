open Cow

let current_module_name = ref ""

let index_content = ref []

let html_reference_of_module sub_module_name =
  let html_page = <:html<$str:(!current_module_name^"."^sub_module_name^".html")$>> in
  <:html<<a href="$html_page$">$str:sub_module_name$</a>&>>

let mark_type = "TYPE"
  
(** The prefix for types elements (record fields or constructors). *)
let mark_type_elt = "TYPEELT"
  
(** The prefix for functions marks. *)
let mark_function = "FUN"

(** The prefix for exceptions marks. *)
let mark_exception = "EXCEPTION"

(** The prefix for values marks. *)
let mark_value = "VAL"

(** The prefix for attributes marks. *)
let mark_attribute = "ATT"

(** The prefix for methods marks. *)
let mark_method = "METHOD"

let create_div s =
  <:html<<div>$str:s$</div> &>>
    
(* parsing error with Cow *)    
let doctype = "<!DOCTYPE HTML>\n"

let character_encoding = 
  <:html<<meta content="text/html; charset=iso-8859-1" http-equiv="Content-Type" />&>>
	
(** The default style options. *)
let default_style_options =
  [ ".keyword { font-weight : bold ; color : Red }" ;
    ".keywordsign { color : #C04600 }" ;
    ".superscript { font-size : 4 }" ;
    ".subscript { font-size : 4 }" ;
    ".comment { color : Green }" ;
    ".constructor { color : Blue }" ;
    ".type { color : #5C6585 }" ;
    ".string { color : Maroon }" ;
    ".warning { color : Red ; font-weight : bold }" ;
    ".info { margin-left : 3em; margin-right: 3em }" ;
    ".param_info { margin-top: 4px; margin-left : 3em; margin-right : 3em }" ;
    ".code { color : #465F91 ; }" ;
    ".typetable { border-style : hidden }" ;
    ".paramstable { border-style : hidden ; padding: 5pt 5pt}" ;
    "tr { background-color : White }" ;
    "td.typefieldcomment { background-color : #FFFFFF ; font-size: smaller ;}" ;
    "div.sig_block {margin-left: 2em}" ;
    "*:target { background: yellow; }" ;

    "body {font: 13px sans-serif; color: black; text-align: left; padding: 5px; margin: 0}";

    "h1 { font-size : 20pt ; text-align: center; }" ;

    "h2 { font-size : 20pt ; border: 1px solid #000000; "^
      "margin-top: 5px; margin-bottom: 2px;"^
      "text-align: center; background-color: #90BDFF ;"^
      "padding: 2px; }" ;

    "h3 { font-size : 20pt ; border: 1px solid #000000; "^
      "margin-top: 5px; margin-bottom: 2px;"^
      "text-align: center; background-color: #90DDFF ;"^
      "padding: 2px; }" ;

    "h4 { font-size : 20pt ; border: 1px solid #000000; "^
      "margin-top: 5px; margin-bottom: 2px;"^
      "text-align: center; background-color: #90EDFF ;"^
      "padding: 2px; }" ;

    "h5 { font-size : 20pt ; border: 1px solid #000000; "^
      "margin-top: 5px; margin-bottom: 2px;"^
      "text-align: center; background-color: #90FDFF ;"^
      "padding: 2px; }" ;

    "h6 { font-size : 20pt ; border: 1px solid #000000; "^
      "margin-top: 5px; margin-bottom: 2px;"^
      "text-align: center; background-color: #90BDFF ; "^
      "padding: 2px; }" ;

    "div.h7 { font-size : 20pt ; border: 1px solid #000000; "^
      "margin-top: 5px; margin-bottom: 2px;"^
      "text-align: center; background-color: #E0FFFF ; "^
      "padding: 2px; }" ;

    "div.h8 { font-size : 20pt ; border: 1px solid #000000; "^
      "margin-top: 5px; margin-bottom: 2px;"^
      "text-align: center; background-color: #F0FFFF ; "^
      "padding: 2px; }" ;

    "div.h9 { font-size : 20pt ; border: 1px solid #000000; "^
      "margin-top: 5px; margin-bottom: 2px;"^
      "text-align: center; background-color: #FFFFFF ; "^
      "padding: 2px; }" ;

    "a {color: #416DFF; text-decoration: none}";
    "a:hover {background-color: #ddd; text-decoration: underline}";
    "pre { margin-bottom: 4px; font-family: monospace; }" ;
    "pre.verbatim, pre.codepre { }";

    ".indextable {border: 1px #ddd solid; border-collapse: collapse}";
    ".indextable td, .indextable th {border: 1px #ddd solid; min-width: 80px}";
    ".indextable td.module {background-color: #eee ;  padding-left: 2px; padding-right: 2px}";
    ".indextable td.module a {color: 4E6272; text-decoration: none; display: block; width: 100%}";
    ".indextable td.module a:hover {text-decoration: underline; background-color: transparent}";
    ".deprecated {color: #888; font-style: italic}" ;

    ".indextable tr td div.info { margin-left: 2px; margin-right: 2px }" ;

    "ul.indexlist { margin-left: 0; padding-left: 0;}";
    "ul.indexlist li { list-style-type: none ; margin-left: 0; padding-left: 0; }";
  ]

(** Style filename *)
let style_file = "style.css"


let style_tag = <:html<
  <link rel="stylesheet" href="$str:style_file$" type="text/css" />
>>

let jquery_url = "http://ajax.googleapis.com/ajax/libs/jquery/1.10.1/jquery.min.js"

let h s = <:html<$str:s$>>

let script body_url =
  let script_content =  h ("$(document).ready(function(){$('#module_body').load('"^body_url^"');})") in
  print_endline (Docjson.string_of_html script_content);
  <:html<<script>$script_content$</script>&>>


(* tmp*)
let create_redirect_page html_page redirect_url =
  let oc = open_out html_page in
  output_string oc doctype;
  let content_content = h ("0; url="^redirect_url) in
  let html_content = 
    let meta = <:html< <meta http-equiv="refresh" content="$content_content$"/> >> in
    <:html<<html><head>$meta$</head><body></body></html>&>> in
  output_string oc (Docjson.string_of_html html_content);
  close_out oc
  
let generate_style_file () =
  if not (Sys.file_exists style_file) then
    let oc = open_out style_file in
    output_string oc (String.concat "\n" default_style_options);
    close_out oc

let generate_head module_name = 
  generate_style_file ();
  <:html<
  <title>$str:module_name$</title>
    $style_tag$
  >>

let make_pre data =
  <:html<<pre>$data$</pre>&>>

let make_span ?css_class data = 
  match css_class with
    | Some clazz -> <:html<<span class="$str:clazz$">$data$</span>&>>
    | None -> <:html<<span>$data$</span>&>>

(** Return html code with the given string in the keyword style.*)
let keyword str =
  make_span ~css_class:"keyword" (h str)

(** Return html code with the given string in the constructor style. *)
let constructor str =
  make_span ~css_class:"constructor" (h str)

(* can't parse optional arg in called f° in html tags *)
let code css_class data =
  match css_class with
      "" -> <:html<<code>$data$</code>&>>
    | css -> <:html<<code class="$str:css$">$data$</code>&>>

let make_info = 
  function 
    | Some i when i != Html.nil -> 
	<:html<<div class="info">$i$</div>&>>
    | _ -> Html.nil
      
let make_type_table f l =
  let table rows =
    <:html<<table class="typetable">
		    $rows$</table>&>> 
  in 
  table 
    (List.fold_left 
       (fun acc x -> <:html<$acc$<tr>$f x$</tr>&>>)
       Html.nil 
       l)

let rec insert_between sep = function
  | [] -> Html.nil
  | [h] -> h
  | h::t -> <:html<$h$$str:sep$$insert_between sep t$>>


let make_field_comment comm =
  <:html<<td class="typefieldcomment" align="left">$comm$</td>&>>

let generate_id_mark mark name html =
  <:html<<span id="$str:mark^name$">$html$</span>&>>

(****************************************)

(* TODO ajouter les id (à voir plus tard) *)

open Docjson

let html_of_value = function 
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

let html_of_type_param_list params variances =
    let lstrparam = (List.map2 
		     (fun param -> function
		       | `None -> param
		       | `Positive -> <:html<+$param$>>
		       | `Negative -> <:html<-$param>>)
		     params variances) in
    match lstrparam with
	[] -> Html.nil
      | [h] -> code "type" <:html<$h$ >>
      | _ -> code "type" <:html<($insert_between ", " lstrparam$) >>

let html_of_type_class_param_list params variances =
  let lstrparam = (List.map2 
		     (fun param -> function
		       | `None -> param
		       | `Positive -> <:html<+$param$>>
		       | `Negative -> <:html<-$param>>)
		     params variances) in
  match lstrparam with
      [] -> Html.nil
    | [h] -> code "type" <:html<[$h$] >>
    | _ -> code "type" <:html<[$insert_between ", " lstrparam$] >>
      
let html_of_type_variant father_name = function 
  | {tk_constructors = Some cstrs; _} ->
    make_type_table 
      (function {vc_name=name; vc_args=args; vc_info=info} ->
	let cell typ = <:html<<td align="left" valign="top"><code>$keyword "|"$</code></td><td align="left" valign="top"><code>$typ$</code></td>&>>
	in 
	let constr_name = 
	  generate_id_mark mark_type_elt (father_name^"."^name)
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
	(* bug on &nbsp -> scotch tape : simple space *)
	<:html<<td align="left" valign="top"><code>  </code></td><td align="left" valign="top"><code>$if mut then keyword "mutable" else Html.nil$ $generate_id_mark mark_type_elt (father_name^"."^name) (h name)$ :$code "type" typ$;</code></td>$match info with Some i -> make_field_comment i | _ -> Html.nil$
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
      <:html<$keyword "type"$ $html_of_type_param_list params variances$$str:name$ >>
    in    
    let name_html = generate_id_mark mark_type name name_html in
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

let html_of_exception = function 
  | { si_item = `Exception; si_name = Some name; 
      si_args = Some args; si_info = info} -> 
    let id = generate_id_mark mark_exception name <:html<$keyword "exception"$ $str:name$>> 
    in let args = match args with 
      | [] -> Html.nil 
      | _ -> <:html< $keyword "of"$ $code "type" (insert_between " * " args)$>> in
    let signature = make_pre <:html<$id$$args$>> in
    <:html<$signature$$make_info info$>>
  | _ -> assert false

(* Modules generation *)
let sub_module_name = ref ""
let sub_module_info = ref None
    
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
    <:html<$args$$code "code" (h "object")$ $ref_link$ $code "code" (h "end")$>>
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
let html_of_class is_class_type = function 
  | { si_name = Some name;
      si_params = Some params;
      si_variance = Some variances;
      si_virt = Some virt;
      si_class_type = Some class_type;
      si_info = info } -> 
    let id = generate_id_mark mark_type name in
    let params_html = html_of_type_class_param_list params variances in
    let ref = <:html<$str:name$>> (* todo : referencing link *) in
    let header = 
      <:html<$keyword (if is_class_type then "class type" else "class")$$
	if virt then keyword " virtual" 
	else Html.nil$ $params_html$$ref$>> in
    let sign = if is_class_type then "=" else ":" in
    let pre = make_pre <:html<$id header$ $str:sign$ $html_of_class_type class_type$>> in
    <:html<$pre$$make_info info$
    >>    
  | _ -> assert false 

(* Do we print normal comments? *)
let html_of_comment = function 
  | { si_item = `Comment; si_info = info} -> 
    (match info with 
      | Some i -> <:html<<br/>$i$<br/>
	  >>
      | None -> Html.nil)
  | _ -> assert false

let generate_page_header info module_name = 
  let title_module_name = 
    String.(let pos = try rindex module_name '.' + 1 with Not_found -> 0 in 
	    sub module_name pos ((length module_name) - pos)) in
  let pre = make_pre 
    (<:html<$keyword "module"$ $str:title_module_name$: $code "code" (h "sig")$ .. $code "code" (h "end")$>>)
  in
  let up_module =
    String.(
      try
	let pos = rindex module_name '.' + 1 in
	sub module_name 0 (pos-1)
      with Not_found -> "index"
    )
  in
  let uplink = h (up_module^".html") in
  let up_module = h up_module in
  let up_nav = <:html<<div class="navbar"><a class="up" href="$uplink$" title="$up_module$">Up</a></div>&>>
  in
  <:html<
    $up_nav$
  <h1>Module $str:module_name$</h1>
    $pre$
    $make_info info$
  >>

let rec html_of_module_ident = function
  | { mt_kind = `Ident; mt_path = Some path} -> 
    <:html<$code "code" path$&>>
  | _ -> <:html<HTML_OF_MODULE_IDENT BUG>>
    (* assert false *)

(** Generate the submodule file at this point *)
and html_of_module_sig = function
  | { mt_kind = `Sig; mt_items = Some items } ->
    let parent_name = !current_module_name in
    current_module_name := parent_name^"."^(!sub_module_name);
    (*print_endline !current_module_name;*)
    generate_html !current_module_name (file items !sub_module_info);
    current_module_name := parent_name;
    <:html<$code "code" (h "sig")$ .. $code "code" (h "end")$>>
  | _ -> assert false

and html_of_module_functor = function 
  | { mt_kind = `Functor
    ; mt_arg_name = Some arg_name
    ; mt_arg_type = Some arg_type
    ; mt_base = Some base} ->
    let path = match arg_type.mt_path with 
      | Some p -> code "type" p
      | None -> Html.nil in (* find a graceful way of including the structural modules *)
    let body = 
      <:html<$code "code" (h "functor (")$$code "code" (h arg_name)$<code class="code"> : </code>$path$$code "code" (h ") -> ")$&>> in
    <:html<<div class="sig_block">$body$$html_body_of_module base$</div>&>>
  | _ -> raise (Failure "html_of_functor: Mismatch")
    
and html_of_module_with = function
  | { mt_kind = `With; 
      mt_cnstrs = Some cnstrs; 
      mt_base = Some base } ->
    let l = List.map 
      (function 
	| {wc_typeq = Some typ; wc_path = path} ->  <:html<type $path$ = $typ$>> 
        | {wc_path = path; wc_modeq = Some modeq} -> <:html<module $path$ = $modeq$>>
	| _ -> assert false)
      cnstrs in
    <:html<$html_body_of_module base$ with $insert_between " and " l$>>
  | _ -> assert false
    

and html_of_module_typeof = function 
  | { mt_kind = `TypeOf; mt_expr = Some ({me_path=Some p; _})} ->
    <:html<module type of $p$>>
  | _ -> assert false

and html_of_module_apply = function
  | { mt_kind = `Apply; 
      mt_arg_type = Some arg_type; 
      mt_base = Some base } -> 
    let base_html = html_of_module_ident base in
    let arg_html = (match arg_type.mt_kind with 
	| `Ident -> html_of_module_ident
	| `Sig -> html_of_module_sig
	| _ -> 
	  (* todo *)
	  (fun x -> <:html<Mismatch module apply>>)
	  (*failwith "html_of_module_apply : Mismatch"*)) arg_type
    in <:html<$base_html$($arg_html$)>>    
  | _ -> assert false

and html_body_of_module mty =
  (match mty.mt_kind with 
  | `Ident   -> html_of_module_ident  
  | `Sig     -> html_of_module_sig
  | `Functor -> html_of_module_functor 
  | `With    -> html_of_module_with   
  | `Apply   -> html_of_module_apply
  | `TypeOf  -> html_of_module_typeof) mty

and html_of_module = function 
  | { si_item = `Module; si_name=Some name; si_module_type=Some mty; si_info=info} ->
    sub_module_name := name;
    sub_module_info := info;
    (* should create the page here *)
    let body = html_body_of_module mty in
    let path = 
      let html_page = !current_module_name^"."^name^".html" in
      (* if it hasn't been created then force the creation with a redirect link 
	 to the parent module *)
      if not (Sys.file_exists html_page) then
	begin
	  create_redirect_page html_page (!current_module_name^".html#");
	  h name
	end
      else
	html_reference_of_module name in
    let sig_mod = make_pre (<:html<$keyword "module"$ $path$ : $body$>>) 
    in
    <:html<$sig_mod$$make_info info$&>>

  | _ -> assert false

(* Module types generation *)
and html_of_modtype = function 
  | { si_item = `ModType
    ; si_name = Some name
    ; si_module_type = Some module_type
    ; si_info = info } -> 
    sub_module_name := name;
    sub_module_info := info;
    let body = html_body_of_module module_type in
    let path = 
      let html_page = !current_module_name^"."^name^".html" in
      (* if it hasn't been created then force the creation with a redirect link 
	 to the parent module *)
      if not (Sys.file_exists html_page) then
	begin
	  create_redirect_page html_page (!current_module_name^".html#");
	  h name
	end
      else
	html_reference_of_module name in
    let sig_mod = make_pre (<:html<$keyword "module type"$ $path$ = $code "type" body$>>)
    in
    <:html<$sig_mod$$make_info info$>>
  | _ -> assert false

and html_of_include = function 
  | { si_item = `Include; si_module_type=Some mty} -> 
    make_pre <:html<$keyword "include"$ $html_body_of_module mty$>>
  | _ -> assert false

and html_of_signature_item sig_item =
	     (match sig_item.si_item with
	       | `Value -> html_of_value
	       | `Primitive -> (fun item -> html_of_value {item with si_item = `Value})
	       | `Type -> html_of_type
	       | `Exception -> html_of_exception
	       | `Module -> html_of_module
	       | `ModType -> html_of_modtype
	       | `Include -> html_of_include
	       | `Class -> html_of_class false
	       | `ClassType -> html_of_class true
	       | `Comment -> html_of_comment) sig_item
	       
and html_of_file module_name =
  function {f_items=sig_items;
	    f_info=info} ->
    let items = List.fold_left
      (fun acc e -> <:html< $acc$ $html_of_signature_item e$ >>)
      Html.nil 
      sig_items in
    let doc_page = (* <!doctype> should be here but parsing doesn't support it *)
      <:html<
      <html>
      <head>
	$generate_head module_name$
      </head>
      <body>
	$generate_page_header info module_name$
      <hr width="100%" /> 
      $items$
      </body>
      </html>
      >>    
    in 
    doc_page

and generate_html ?(is_root_module=false) module_name jfile =
 if is_root_module then
   add_to_index module_name jfile.f_info;

  current_module_name := module_name;
  let html = html_of_file module_name jfile in
  let html_name = module_name ^ ".html" in
  let oc = open_out html_name in
  output_string oc doctype;
  output_string oc (Docjson.string_of_html html);
  close_out oc


(* Il me faut une fonction pour : 
   - générer les body
   - générer la fake page
   - générer le script
   - 
*)

and add_to_index m_name info =
  let html_page = m_name^".html" in
  let content = 
    <:html<<tr><td class="module"><a href="$str:html_page$">$str:m_name$</a></td><td>
		     $make_info info$
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
	    (List.sort (fun (n,_) (n2, _) -> compare n n2) l)
	in
	let oc = open_out "index.html" in
	output_string oc doctype;
	let html_content = 
	  <:html<
	  <html>
	  <head> 
	  <link rel="stylesheet" href="$str:style_file$" type="text/css" />
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

let print_error_page m_name =
  let oc = open_out (m_name^".html") in
  output_string oc doctype;
  let html_content = 
    <:html<
    <html>
      <head><title>:(</title></head>
    <body>
      Error - Couldn't generate the documentation.<br/>
      Are the cmt/cmd correctly built? 
    </body>
    </html>&>> in
    output_string oc (string_of_html html_content);
    close_out oc
      
