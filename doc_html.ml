open Cow

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

let character_encoding = <:html<
  <meta content="text/html; charset=iso-8859-1" http-equiv="Content-Type" /> 
	       &>>
	
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

let h s = <:html<$str:s$>>
  
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
  <:html<<td class="typefieldcomment" align="left" >$comm$</td>&>>

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

(* TODO *)
let html_of_primitive = function 
  | { si_item = `Primitive; _ } -> Html.nil
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
	| Some typ,`Record -> <:html<= {$typ$ >>
        | Some typ,_ -> <:html<= $typ$ >>
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

(* TODO *)
let html_of_exception = function 
  | { si_item = `Exception; _ } -> <:html<TODO exception>>
  | _ -> assert false

let html_of_module_ident = function
  | { mt_kind = `Ident; mt_path = Some path} -> Html.nil
  | _ -> assert false

let html_of_module_sig = function
  | { mt_kind = `Sig; mt_items = Some items } -> Html.nil
  | _ -> assert false

let html_of_module_functor = function
{ mt_kind = `Functor; mt_arg_name = Some arg_name; mt_arg_type = Some arg_type; mt_base = Some base} -> Html.nil
  | _ -> assert false

let html_of_module_with = function
  | { mt_kind = `With; mt_cnstrs = Some cnstrs; mt_base = Some base } -> Html.nil
  | _ -> assert false

let html_of_module_typeof = function 
  | { mt_kind = `TypeOf; mt_expr = Some expr } -> Html.nil
  | _ -> assert false

let html_of_module = function 
  | { si_item = `Module; si_module_type=Some mty} -> 
    (match mty.mt_kind with
      | `Ident   -> html_of_module_ident  
      | `Sig     -> html_of_module_sig 	  
      | `Functor -> html_of_module_functor 
      | `With    -> html_of_module_with   
      | `TypeOf  -> html_of_module_typeof) mty
  | _ -> assert false

(* TODO *)
let html_of_modtype = function 
  | { si_item = `ModType; _ } -> <:html<TODO module type>>
  | _ -> assert false

(* TODO *)
let html_of_include = function 
  | { si_item = `Include; _ } -> <:html<TODO include>>
  | _ -> assert false

(* TODO *)
let html_of_class = function 
  | { si_item = `Class; _ } -> <:html<TODO class>>
  | _ -> assert false

(* TODO *)
let html_of_classtype = function 
  | { si_item = `ClassType; _ } -> <:html<TODO class>>
  | _ -> assert false

(* Do we print comment? *)
let html_of_comment = function 
  | { si_item = `Comment; si_info = Some info} -> info
  | _ -> assert false

let html_of_signature_item sig_item =
  match sig_item.si_item with
    | `Value -> html_of_value sig_item
    | `Primitive -> html_of_primitive sig_item
    | `Type -> html_of_type sig_item
    | `Exception -> html_of_exception sig_item
    | `Module -> html_of_module sig_item	 
    | `ModType -> html_of_modtype sig_item
    | `Include -> html_of_include sig_item
    | `Class -> html_of_class sig_item
    | `ClassType -> html_of_classtype sig_item
    | `Comment -> html_of_comment sig_item
    
let generate_page_header info module_name = 
  (* link on current page *)
  let pre = make_pre 
    (<:html<$keyword "module"$ $str:module_name$: $code "code" (h "sig")$ .. $code "code" (h "end")$>>) 
  in
  <:html<
  <h1>Module $str:module_name$</h1>
    $pre$
    $make_info info$
  >>

let html_of_file module_name =
  function {f_items=sig_items;
	    f_info=info} ->
    let items = List.fold_left
      (fun acc e -> <:html< $acc$ $html_of_signature_item e$ >>)
      Html.nil 
      sig_items in
    (* <!doctype> should be here but parsing doesn't support it *)
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
