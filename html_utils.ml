open Cow

let fold_html =
  List.fold_left
    (fun acc elem ->
      <:html<$acc$
$elem$>>) Html.nil

let string_of_html = Docjson.string_of_html

let html_of_string = Html.of_string

(** {3 Tags generators} *)

module TagsGenerators = struct
let make_info = 
  function 
    | Some i when i != Html.nil -> 
	<:html<<div class="info">$i$</div>&>>
    | _ -> Html.nil

 (* quick hack to slice comments for index description
    stop at the first dot or at the first newline *)
 let make_first_line_info = function 
   | Some i when i != Html.nil -> 
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
     
     <:html<<div class="info">$html_of_string (String.sub info 0 idx)$</div>&>>
   | _ -> Html.nil
     
let make_pre data =
  <:html<<pre>$data$</pre>&>>

let make_span ?css_class data = 
  match css_class with
    | Some clazz -> <:html<<span class="$str:clazz$">$data$</span>&>>
    | None -> <:html<<span>$data$</span>&>>

(** Return html code with the given string in the keyword style.*)
let keyword str =
  make_span ~css_class:"keyword" (html_of_string str)

(** Return html code with the given string in the constructor style. *)
let constructor str =
  make_span ~css_class:"constructor" (html_of_string str)

(* can't parse optional arg in called f° in html tags *)
let code css_class data =
  match css_class with
      "" -> <:html<<code>$data$</code>&>>
    | css -> <:html<<code class="$str:css$">$data$</code>&>>
      
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

let make_reference name path = 
  <:html<<a href="$str:path$">$str:name$</a>&>>
      
let create_content_to_load_tag content_path =
  <:html<<div file="$str:content_path$" class="$str:Opam_doc_config.content_to_load_class$"> </div>&>>

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
    | [h] -> code "type" <:html<[$h$] >> (* add some brackets ~~ *)
    | _ -> code "type" <:html<[$insert_between ", " lstrparam$] >>


let generate_id_mark mark name html =
  <:html<<span id="$str:mark^name$">$html$</span>&>>

 (*  :lllll *)
let wrap_ident_module signature module_name path =
  match path with 
    | Some p -> 
      <:html<<div class="ocaml_module ident" name="$str:module_name$" path=$uri:Uri.of_string p$>
		    $signature$
</div>&>>
    | None ->       
      <:html<<div class="ocaml_module ident" name="$str:module_name$">
		    $signature$
</div>&>>


let wrap_sig_module signature elements module_name =
  <:html<<div class="ocaml_module sig" name="$str:module_name$">
		$signature$
		<div class="ocaml_module_content">
		       $elements$
		</div>
</div>&>>

let wrap_include include_item path sig_items signature =
  let mod_type = Index.lookup_include_module_type include_item in
  let included_items = 
    let open Types in 
	let msig = match mod_type with 
	  | Mty_signature msig -> msig | _ -> [] 
	in
	List.fold_left 
	  (fun acc -> function 
	  | Sig_module (id, _, _) | Sig_modtype (id, _) 
	  | Sig_class (id, _, _) | Sig_class_type (id, _, _) 
	    -> ("\"" ^ id.Ident.name ^ "\"") ::acc
	  | _ ->acc)	   
	[] msig
    in
    let js_array = "[" ^ String.concat "," included_items ^ "]" in
    match path with
      |	Some p ->
	<:html<<div class="ocaml_include ident" path=$uri:Uri.of_string p$ items="$str:js_array$">
		      $signature$
</div>&>>
      | None ->
	let module_content = match sig_items with 
	  |Some l -> <:html<<div class="ocaml_module_content">
			    $l$</div>&>> 
	  | None -> Html.nil in
	<:html<<div class="ocaml_include sig" items="$str:js_array$">
		      $signature$$module_content$
</div>&>>

end


open TagsGenerators		

(** {3 Html pages generators} *)

let create_html_skeleton filename (headers : Html.t list) (body : Html.t list) =
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

let create_html_default_page global filename title =
  create_html_default_skeleton filename title 
    (* package index - links + comm *)
    (List.map 
    (fun package -> 
      let uri = Uri.of_string ("?package=" ^ package) in
      <:html<<a href=$uri:uri$>$str:String.capitalize package$</a><br/>&>>)
    (List.sort String.compare (Index.get_global_packages global)))
      
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

let rec grab_base_module = 
  let open Docjson in
  function
    | {mt_kind=`Ident; _}
    | {mt_kind=`Sig; _} as mty -> mty
    | {mt_kind=`Functor; mt_base=Some base; _}
    | {mt_kind=`With; mt_base=Some base; _}
    | {mt_kind=`Apply; mt_base=Some base; _} -> grab_base_module base
    | {mt_kind=`TypeOf; mt_expr = Some {me_path=Some p}; _} -> kModTypeIdent p
    | _ -> assert false


let flatten_symlinks () =
  let open Unix in 
      let flatten_link linkfile =
	let rec loop target =
	  try
	    let target_link = readlink target in
	    loop target_link
	  with	      
	    | Unix_error (EINVAL, "readlink", _) ->
	      target
	in
	let target = loop linkfile in
	unlink linkfile;
	symlink target linkfile
      in
      let curr_dir = opendir !Opam_doc_config.current_package in
      try
	while true do
	  try 
	    let curr_file  = readdir curr_dir in
	    match (lstat curr_file).st_kind with
	      | S_LNK -> flatten_link curr_file
	      | _ -> () (* Link not built yet? *)
	  with
	    | Unix_error _ -> ()
	done
      with
	  End_of_file -> closedir curr_dir
