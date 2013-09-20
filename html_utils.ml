(** {3 Tags generators} *)

let make_info = 
  function 
    | Some i when i != Cow.Html.nil -> 
      <:html<<div class="info">$i$</div>&>>
    | _ -> Cow.Html.nil

let make_span ?css_class data = 
  match css_class with
    | Some clazz -> <:html<<span class="$str:clazz$">$data$</span>&>>
    | None -> <:html<<span>$data$</span>&>>

let keyword str =
  make_span ~css_class:"keyword" (Html.html_of_string str)

let make_type_table l =
  let table rows =
    <:html<<table class="typetable">
		    $rows$</table>&>> 
  in 
  table 
    (List.fold_left 
       (fun acc x -> <:html<$acc$<tr>$x$</tr>&>>)
       Cow.Html.nil 
       l)

(* Cannot lookup by #id, another type with the same id  could be found in submodules *)
let generate_mark mark name html =
  let open Opam_doc_config in
      let mark_id = match mark with
	| Attribute -> "ATT"
	| Type -> "TYPE"
	| Type_elt -> "TYPEELT"
	| Function -> "FUN"
	| Exception -> "EXCEPTION"
	| Value -> "VAL"
	| Method -> "METHOD"
	| Title -> "TITLE" 
      in
  <:html<<span class="$str:mark_id^name$">$html$</span>&>>

(* Beginning of generate-utils *)

let make_field_comment comm =
  <:html<<td class="typefieldcomment" align="left">$comm$</td>&>>

let make_variant_cell parent_name name args_type info = 
  let html_name = make_span ~css_class:"constructor" (Html.html_of_string name) in
  let html_name = generate_mark 
    Opam_doc_config.Type_elt
    (parent_name^"."^name) html_name in
  let html_body = match args_type with
    | [] -> <:html<$html_name$>>
    | _ -> let l = Html.concat ~sep:" * " args_type in
           let cd = Html.code ~cls:"type" l in
	   <:html<$html_name$ $keyword "of"$ $cd$>>
  in
  let info_td = match info with 
    | Some i -> make_field_comment i
    | _ -> Cow.Html.nil in
  <:html<<td align="left" valign="top"><code>$keyword "|"$</code></td><td align="left" valign="top"><code>$html_body$</code></td>$info_td$&>>

let make_record_label_cell parent_name name is_mutable label_type info = 
  let spacing_td = <:html<<td align="left" valign="top"><code>  </code></td>&>> in
  
  let html_name = if is_mutable then 
      <:html<$keyword "mutable"$ >> (* space is important here *)
    else Cow.Html.nil in
  let marked_name = generate_mark 
    Opam_doc_config.Type_elt
    (parent_name^"."^name) (Html.html_of_string name) in
  let html_name = <:html<$html_name$$marked_name$>> in

  let body_td = 
    let cd = Html.code ~cls:"type" label_type in
    <:html<<td align="left" valign="top"><code>$html_name$ : $cd$;</code></td>&>> in
  
  let info_td = match info with 
	       | Some i -> make_field_comment i
	       | _ -> Cow.Html.nil in
  
  <:html<$spacing_td$$body_td$$info_td$>>

let create_module_signature_content elements = 	
  let elems = Html.concat (Cow.Html.nil :: elements) in
  <:html<<div class="ocaml_module_content">$elems$</div>&>>

let create_class_signature_content elements = 	
  let elems = Html.concat elements in
  <:html<<div class="ocaml_class_content">$elems$</div>&>>

let create_class_container class_name signature html_content = function
  | Some (Gentyp.Unresolved _) -> 
    <:html<<div class="ocaml_class ident" name="$str:class_name$">$signature$$html_content$</div>&>>
  | Some (Gentyp.Resolved (uri, _)) ->
    <:html<<div class="ocaml_class ident" name="$str:class_name$" path="$uri:uri$"> $signature$$html_content$</div>&>>
  | None -> 
    <:html<<div class="ocaml_class sig" name="$str:class_name$">$signature$$html_content$</div>&>>
  | Some (Gentyp.Apply _) -> assert false


(* End of generate-utils *)

let html_of_type_param_list params variances =
  let lstrparam = (List.map2 
		     (fun param -> function
		       | `None -> param
		       | `Positive -> <:html<+$param$>>
		       | `Negative -> <:html<-$param>>)
		     params variances) in
    match lstrparam with
	[] -> Cow.Html.nil
      | [h] -> Html.code ~cls:"type" <:html<$h$ >>
      | _ -> let h = Html.concat ~sep:", " lstrparam in
             Html.code ~cls:"type" <:html<($h$) >>

let html_of_type_class_param_list params variances =
  let lstrparam = (List.map2 
		     (fun param -> function
		       | `None -> param
		       | `Positive -> <:html<+$param$>>
		       | `Negative -> <:html<-$param>>)
		     params variances) in
  match lstrparam with
      [] -> Cow.Html.nil
    | [h] -> Html.code ~cls:"type" <:html<[$h$] >> (* add some brackets ~~ *)
    | _ -> let h = Html.concat ~sep:", " lstrparam in
           Html.code ~cls:"type" <:html<[$h$] >>

  
let js_array_of_include_items = 
  let open Types in function
    | Mty_signature msig -> 
      let included_items = List.fold_left
	(fun acc -> function 
	  | Sig_module (id, _, _) | Sig_modtype (id, _) 
	  | Sig_class (id, _, _) | Sig_class_type (id, _, _) 
	    -> ("\"" ^ id.Ident.name ^ "\"") ::acc
	  | _ ->acc)	   
	[] msig 
      in
      "[" ^ String.concat "," included_items ^ "]"
    | _ -> "[]"

(** {3 Html pages generators} *)

let create_html_skeleton filename (headers : Cow.Html.t list) (body : Cow.Html.t list) =
  let oc = open_out filename in
  let header_elements = Html.concat headers in
  let body_elements = Html.concat body in
  output_string oc Opam_doc_config.doctype;
  let page =
    <:html<<html>
<head>
$header_elements$
</head>
<body>
$body_elements$
</body>
</html>&>> in
  begin
    output_string oc (Html.string_of_html page);
    close_out oc
  end
    

let create_html_default_skeleton filename title body_list =
  let headers = 
    let open Opam_doc_config in 
	[ <:html<<title>$str:title$</title>&>>
	; character_encoding
	; style_tag
	; script_tag 
	] 
    in
    create_html_skeleton filename headers body_list
      
(** Hacks functions *)

let write_unless_exists ~filename ~contents =
  if not (Sys.file_exists filename) then
    begin
      let oc = open_out filename in
      output_string oc contents;
      close_out oc
    end

let output_style_file () =
  write_unless_exists
    ~filename:Opam_doc_config.style_filename
    ~contents:Opam_doc_config.default_stylesheet

let output_script_file () =
  write_unless_exists
    ~filename:Opam_doc_config.script_filename
    ~contents:Opam_doc_config.default_script

let generate_package_index = function
  | [] -> ()
  | l ->
    let make_content (m_name, info) = 
      let uri = Uris.module_uri m_name in
      <:html<<tr><td class="module"><a href="$uri:uri$">$str:m_name$</a></td><td>$info$</td></tr>&>> 
    in
    let oc = open_out (Opam_doc_config.current_package () ^ "/index.html") in
    let content = Html.concat (List.map make_content l) in
    let html_content =
      <:html<<h1>Modules</h1>
<table class="indextable">
    $content$
</table>&>>
    in
    output_string oc (Html.string_of_html html_content);
    close_out oc

let generate_global_packages_index global = 
  let packages = Index.get_global_packages global in
  let generate_package_entry (package_name, info) = 
    let uri = Uris.package_uri package_name in
    <:html<<tr><td class="module"><a href="$uri:uri$">$str:String.capitalize package_name$</a></td><td>$opt:info$</td></tr>&>>
  in
  let h = Html.concat (List.map generate_package_entry packages) in
  let html_body = 
    <:html<<h1>Packages list</h1>
<table class="indextable">
$h$
</table>&>> in
  create_html_default_skeleton (Opam_doc_config.default_index_name ()) "Opam-Doc" [html_body]
