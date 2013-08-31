let fold_html =
  List.fold_left
    (fun acc elem ->
      <:html<$acc$
$elem$>>) Cow.Html.nil

let string_of_html = Cow.Html.to_string

let html_of_string = Cow.Html.of_string ~enc:`UTF_8

(** {3 Tags generators} *)

let make_info = 
  function 
    | Some i when i != Cow.Html.nil -> 
      <:html<<div class="info">$i$</div>&>>
    | _ -> Cow.Html.nil

 (* quick hack to slice comments for index description
    stop at the first dot or at the first newline *)
 let make_first_line_info = function 
   | Some i when i != Cow.Html.nil -> 
     let info = string_of_html i in
     (* should look for <p> but cow doesn't support mandatory tag policies *)
     let rec index_before_br_tag idx str =
       let i = String.index_from str idx '<' in
       if str.[i+1] = 'b' && str.[i+2] = 'r' then
	 i-1
       else
	 index_before_br_tag (i+1) str
     in
     let rec index_after_a_dot_with_a_space idx str = 
       let i = String.index_from info idx '.' + 1 in
       try 
	 if str.[i] = ' ' || str.[i] = '<' then
	   i
	 else
	   index_after_a_dot_with_a_space i str
       with 
	   Invalid_argument _ -> i (* out of bounds *)
     in
     let idx_dot = try index_after_a_dot_with_a_space 0 info 
       with Not_found -> String.length info 
     in
     let idx_newline = try index_before_br_tag 0 info 
       with Not_found -> String.length info in
     let idx = min idx_dot idx_newline in
     
     html_of_string (String.sub info 0 idx)
   | _ -> Cow.Html.nil
     
let make_pre data =
  <:html<<pre>$data$</pre>&>>

let make_span ?css_class data = 
  match css_class with
    | Some clazz -> <:html<<span class="$str:clazz$">$data$</span>&>>
    | None -> <:html<<span>$data$</span>&>>

let keyword str =
  make_span ~css_class:"keyword" (html_of_string str)

let constructor str =
  make_span ~css_class:"constructor" (html_of_string str)

(* can't parse optional arg in called f° in html tags *)
let code css_class data =
  match css_class with
      "" -> <:html<<code>$data$</code>&>>
    | css -> <:html<<code class="$str:css$">$data$</code>&>>
      
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

let rec insert_between ~sep:sep = function
  | [] -> Cow.Html.nil
  | [h] -> h
  | h::t -> <:html<$h$$str:sep$$insert_between sep t$>>

(* Beginning of generate-utils *)

let make_field_comment comm =
  <:html<<td class="typefieldcomment" align="left">$comm$</td>&>>

let make_variant_cell parent_name name args_type info = 
  let html_name = make_span ~css_class:"constructor" (html_of_string name) in
  let html_name = generate_mark 
    Opam_doc_config.Type_elt
    (parent_name^"."^name) html_name in
  
  let html_body = match args_type with
    | [] -> Cow.Html.nil
    | _ -> let l = insert_between " * " args_type in
	   <:html<$html_name$ $keyword "of"$ $code "type" l$>>
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
    (parent_name^"."^name) (html_of_string name) in
  let html_name = <:html<$html_name$$marked_name$>> in

  let body_td = 
    <:html<<td align="left" valign="top"><code>$html_name$ : $code "type" label_type$</code></td>&>> in
  
  let info_td = match info with 
	       | Some i -> make_field_comment i
	       | _ -> Cow.Html.nil in
  
  <:html<$spacing_td$$body_td$$info_td$>>

let create_module_signature_content elements = 	
  <:html<<div class="ocaml_module_content">$fold_html elements$</div>&>>

let create_class_signature_content elements = 	
  <:html<<div class="ocaml_class_content">$fold_html elements$</div>&>>

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
      [] -> Cow.Html.nil
    | [h] -> code "type" <:html<[$h$] >> (* add some brackets ~~ *)
    | _ -> code "type" <:html<[$insert_between ", " lstrparam$] >>


  
let rec js_array_of_include_items = 
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
  let header_elements = fold_html headers in
  let body_elements = fold_html body in
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
    output_string oc (string_of_html page);
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

(* To be removed at some point *)

(** Extract the reference from a '<a .* href="...".*</a>' tag *)
let extract_path path = 
  (* there are some \n generated by the string_of_html *)
  let s = String.trim (string_of_html path) in
  if s.[0] = '<' && s.[1] = 'a' then
    let open String in
	let i = index s '"' + 1 in
	let j = index_from s i '"' in
	sub s i (j-i)
  else
    raise Not_found


(** Extract the innerHTML from a '<a .*> ... </a>' tag or identity if <a is not matched *)
let extract_name path = 
  (* there could be some \n generated by the string_of_html *)
  let s = String.trim (string_of_html path) in
  if s.[0] = '<' && s.[1] = 'a' then
    let open String in
	let i = index s '>' + 1 in
	let j = index_from s i '<' in
	sub s i (j-i)
  else
    s


let output_style_file () =
  if not (Sys.file_exists Opam_doc_config.style_filename) then
    begin
      let oc = open_out Opam_doc_config.style_filename in
      output_string oc (String.concat "\n" Opam_doc_config.default_stylesheet);
      close_out oc
    end

let output_script_file () =
  if not (Sys.file_exists Opam_doc_config.script_filename) then
    begin
      let oc = open_out Opam_doc_config.script_filename in
      output_string oc Opam_doc_config.default_script;
      close_out oc
    end

let generate_package_index = function
  | [] -> ()
  | l ->
    let make_content (m_name, info) = 
      let uri = Uri.of_string 
	("?package="^ !Opam_doc_config.current_package ^"&module="^m_name) in
      <:html<<tr><td class="module"><a href="$uri:uri$">$str:m_name$</a></td><td>$info$</td></tr>&>> 
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

let generate_global_packages_index global = 
  let packages = Index.get_global_packages global in
  let generate_package_entry (package_name, info) = 
    let uri = Uri.of_string ("?package="^package_name) in
    <:html<<tr><td class="module"><a href="$uri:uri$">$str:String.capitalize package_name$</a></td><td>$opt:info$</td></tr>&>>
  in
  let html_body = 
    <:html<<h1>Packages list</h1>
<table class="indextable">
$fold_html (List.map generate_package_entry packages)$
</table>&>> in
  create_html_default_skeleton  !Opam_doc_config.default_index_name "Opam-Doc" [html_body]
