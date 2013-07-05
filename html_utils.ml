open Cow

let fold_html =
  List.fold_left
    (fun acc elem ->
      <:html<$acc$
$elem$>>) Html.nil

let string_of_html = Docjson.string_of_html

let html_of_string s = <:html<$str:s$>>

(** {3 Tags generators} *)

module TagsGenerators = struct
let make_info = 
  function 
    | Some i when i != Html.nil -> 
	<:html<<div class="info">$i$</div>&>>
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
    

let create_html_default_skeleton filename module_name body_list =
  let headers = 
    let open Opam_doc_config in 
	[ <:html<<title>$str:module_name$</title>&>>
	; character_encoding
	; style_tag
	; script_tag 
	] 
    in
    create_html_skeleton filename headers body_list
		

(* TODO : add the possible reference to identifier module *)
(* html_signature =
   module M = S | module M = struct .. end 
   default : module M
*)   
let create_module_body_title module_name title_signature info =  
  let html_signature = 
    match title_signature with Some s -> s 
      | None -> <:html<<pre>$keyword "module"$ $str:module_name$</pre>&>>
  in
  <:html<<h1>Module $str:module_name$</h1>
$html_signature$$make_info info$
<hr/>&>>


  let create_module_body_title_with_ref module_name target_name target_path  =  
    let target_name = Filename.basename target_name in
    <:html<<h1>Module $str:module_name$ = <a href="$str:target_path$">$str:target_name$</a></h1>
<hr/>&>>


(** Output an .html file to a concrete ocaml module definition.
    It will also contains the html tag with the path referencing
    the .html.contents file to load

    filename = ".*.html"
    title_signature : Html.t
    module_name = "A.B.M"
    info : Html.t
*)    
let create_html_concrete_page filename title_signature module_name info =
  let contents_filename = filename ^ Opam_doc_config.page_contents_extension in
  create_html_default_skeleton 
    filename module_name 
    [ create_module_body_title module_name title_signature info
    ; create_content_to_load_tag contents_filename
    ]

let create_html_concrete_page_with_ref filename module_name target_name target_path =
  let contents_filename = filename ^ Opam_doc_config.page_contents_extension in
  create_html_default_skeleton 
    filename module_name 
    [ create_module_body_title_with_ref module_name target_name target_path
    ; create_content_to_load_tag contents_filename
    ]

(** Output an .contents file containing the concrete
    module documentation and the possible recursive
    tags to .contents *)
let create_html_page_contents filename (items : Html.t list) =
  let contents_filename = filename ^ Opam_doc_config.page_contents_extension in
  let oc = open_out contents_filename in
  begin
    List.iter (fun item -> output_string oc ((string_of_html item)^"\n")) items;
    close_out oc
  end

let create_html_symlink_contents filename target =
  let contents_filename = filename ^ Opam_doc_config.page_contents_extension in
  let contents_target_filename = target ^ Opam_doc_config.page_contents_extension in
  Unix.(
    try
      symlink contents_target_filename contents_filename
    with
	Unix_error _ ->
	  unlink contents_filename;
	  symlink contents_target_filename contents_filename
  )

(* str_env : "M.M'.M''" *)
(* path : .*/bla.html *)
let create_include_pages str_env path (module_type : Types.module_type)  =
  let trunc_path = Filename.chop_suffix path ".html" in
  let open Types in
      let rec loop env path = function
	| Mty_signature msig -> 
	  ListLabels.iter msig 
	    ~f:(function
	      | Sig_module (id, msig,_) 
	      | Sig_modtype (id, Modtype_manifest (msig)) -> 		
		let new_env = env^"."^id.Ident.name in
		let new_path = path^"."^id.Ident.name in
		
		(* .contents creation *)
		create_html_symlink_contents 
		  (new_env^".html") (new_path^".html");
		
		(* .html creation *)
		create_html_concrete_page_with_ref 
		  (new_env^".html")
		  new_env
		  new_path
		  (new_path^".html");
		
		loop new_env new_path msig 
	      | Sig_class _
	      | Sig_class_type _ -> ()

	      | Sig_modtype _ 
	      | Sig_value _
	      | Sig_type _ 
	      | Sig_exception _ -> ()
	    );
	
	| Mty_functor _
	| Mty_ident _ -> assert false
      in
      loop str_env trunc_path module_type
      
(** Hacks functions *)

(* To be removed at some point *)
(** Extract the reference from a '<a .* href="...".*</a>' tag *)
let extract_path path = 
  let s = string_of_html path in
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
