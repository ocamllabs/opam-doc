(** {3 Tags generators} *)

let make_info = 
  function 
    | Some i when i <> Cow.Html.nil -> 
      <:html<<div class="info">$i$</div>&>>
    | _ -> Cow.Html.nil

let opt_to_nil = function
    Some h -> h
  | None -> Cow.Html.nil

let make_span ?css_class data = 
  match css_class with
    | Some clazz -> <:html<<span class="$str:clazz$">$data$</span>&>>
    | None -> <:html<<span>$data$</span>&>>

let make_summary i = <:html<<div class="ocaml_summary">$i$</div>&>>

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

let create_signature_content elements = 	
  let elems = Html.concat (Cow.Html.nil :: elements) in
  <:html<<div class="ocaml_content">$elems$</div>&>>

let create_signature_content elements = 	
  let elems = Html.concat (Cow.Html.nil :: elements) in
  <:html<<div class="ocaml_content">$elems$</div>&>>

let create_class_signature_content elements = 	
  let elems = Html.concat elements in
  <:html<<div class="ocaml_content">$elems$</div>&>>

let create_class_container class_name signature html_content = function
  | Some (Gentyp.Unresolved _) -> 
    <:html<<div class="ocaml_class" name="$str:class_name$">$signature$$html_content$</div>&>>
  | Some (Gentyp.Resolved (uri, _, _)) ->
    <:html<<div class="ocaml_class" name="$str:class_name$" path="$uri:uri$"> $signature$$html_content$</div>&>>
  | None -> 
    <:html<<div class="ocaml_class" name="$str:class_name$">$signature$$html_content$</div>&>>
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

  
let js_array_of_include_items msig = 
  let open Types in
  let included_items = List.fold_left
    (fun acc -> function 
      | Sig_module (id, _, _) | Sig_modtype (id, _) 
      | Sig_class (id, _, _) | Sig_class_type (id, _, _) 
        -> ("\"" ^ id.Ident.name ^ "\"") :: acc
      | _ -> acc)	   
    [] msig 
  in
    "[" ^ String.concat "," included_items ^ "]"

let js_array_of_include_types msig = 
  let open Types in
  let included_types = List.fold_left
    (fun acc -> function 
      | Sig_type (id, _, _) -> ("\"" ^ id.Ident.name ^ "\"") ::acc
      | _ -> acc)	   
    [] msig 
  in
    "[" ^ String.concat "," included_types ^ "]"

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

let generate_package_summary filename = function
  | [] -> ()
  | l ->
    let make_content (m_name, info) = 
      let uri = Uris.uri [m_name, Uris.Module] in
      <:html<<tr><td class="module"><a href="$uri:uri$">$str:m_name$</a></td><td>$info$</td></tr>&>> 
    in
    let oc = open_out filename in
    let content = Html.concat (List.map make_content l) in
    let html_content =
      <:html<<h2>Modules</h2>
<table class="indextable">
    $content$
</table>&>>
    in
    output_string oc (Html.string_of_html html_content);
    close_out oc

let page ~title ~headers ~content =
  (* Cannot be inlined below as the $ is interpreted as an antiquotation *)
  let js_init = [`Data "$(document).foundation(); hljs.initHighlightingOnLoad();"] in
  let body =
  <:html<
    <head>
      <meta charset="utf-8" />
      <meta name="viewport" content="width=device-width"/>
      <title>$str:title$</title>
      <link rel="stylesheet" href="/css/foundation.min.css"> </link>
      <link rel="stylesheet" href="/css/magula.css"> </link>
      <link rel="stylesheet" href="/css/site.css"> </link>
      <script src="/js/vendor/custom.modernizr.js"> </script>
      $headers$
    </head>
    <body>
      $content$
      <script src="/js/vendor/jquery.js"> </script>
      <script src="/js/foundation.js"> </script>
      <script src="/js/foundation/foundation.topbar.js"> </script>
      <script src="/js/vendor/highlight.pack.js"> </script>
      <script> $js_init$ </script>
    </body>
  >> in
  Printf.sprintf "\
  <!DOCTYPE html>
  <!--[if IE 8]><html class=\"no-js lt-ie9\" lang=\"en\" ><![endif]-->
  <!--[if gt IE 8]><!--><html class=\"no-js\" lang=\"en\" ><!--<![endif]-->
  %s
</html>" (Cow.Html.to_string body)

let generate_package_index filename =
  let open Opam_doc_config in
  let title = current_package () in
  let headers =
    <:html<
      $character_encoding$
      $style_tag ()$
      $config_tag ()$
      $script_tag ()$
    >> in
  let content = [] in
  let page = page ~title ~headers ~content in
  let oc = open_out filename in
    output_string oc doctype;
    output_string oc page;
    close_out oc

let generate_global_packages_index global = 
  let packages = Index.get_global_packages global in
  let generate_package_entry (package_name, info) = 
    let uri = Uris.package_uri package_name in
    <:html<<tr><td class="module"><a href="$uri:uri$">$str:String.capitalize package_name$</a></td><td>$opt:info$</td></tr>&>>
  in
  let oc = open_out (Opam_doc_config.default_index_name ()) in
  let content = Html.concat (List.map generate_package_entry packages) in
  let open Opam_doc_config in
  let title = "Packages" in
  let headers = <:html< $character_encoding$ $style_tag ()$ >> in
  let content = <:html< 
    <div class="ocaml_head">
    <h1 class="ocaml_title">Packages list</h1>
    </div>
    <hr/>
   <div class="ocaml_body">
   <table class="indextable">
    $content$
   </table>
</div> >> in
  page ~title ~headers ~content
  |> output_string oc;
  close_out oc

(* Module description shortener *)

let find_end_of_sentence s =
  let i = ref 0 in
  let len = String.length s in
  try
    while true do
      i := String.index_from s !i '.';
      incr i;
      if !i < (len - 1) then
	begin
	  match s.[!i] with
	    | '\n' | '\t' | ' ' -> raise Exit
	    | _ -> ()
	end
    done;
    assert false
  with  
    | Exit -> String.sub s 0 !i

(** Info.t -> Info.t *)
let cut_first_sentence info =
  let open Info in
      match info.i_desc with
	| None -> Info.dummy
	| Some text ->
	  let rec loop acc = function
	    | ((Raw s) as e)::t -> 
	      begin
		try		
		  let s = find_end_of_sentence s in
		  List.rev (Raw s::acc)
		with
		  | Not_found -> loop (e::acc) t
	      end
	    | [] | Newline::_ -> List.rev acc
	    | h::t -> loop (h::acc) t
	  in
	  {Info.dummy with i_desc=Some (loop [] text)}	    
