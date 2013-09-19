open Info
open Doctree
open Typedtree
open Location
open Asttypes

open Types

type signature = Cow.Html.t
type module_content = Cow.Html.t    

type structure_result = 
  | Ident of signature * Gentyp.path
  | Sig of signature * module_content

let get_path local ?(is_class=false) (p : Path.t) =
  Gentyp.path local is_class p
    
let path_to_html = Gentyp.html_of_path

let generate_html_path local ?(is_class=false) (p : Path.t) : Cow.Html.t =
  let path = get_path local ~is_class:is_class p in
  Gentyp.html_of_path path

(* => non reentrant *)
let glob_env = ref [] 

let get_full_path_name () =
  String.concat "." !glob_env

let current_class_uri name =
  let modpath = get_full_path_name () in
  Uris.class_uri modpath name

let current_module_uri () =
  let modpath = get_full_path_name () in
  Uris.module_uri modpath
    
let add_internal_reference id =
   Index.add_internal_reference id !glob_env

let rec add_include_references sig_list = 
  let rec treat_module_type id = function
    | Mty_ident p -> () (* should really do something ? *)
    | Mty_signature msig -> 
      let prev_env = !glob_env in 
      glob_env := !glob_env @ [id.Ident.name];      
      add_include_references msig;
      glob_env := prev_env;
    | Mty_functor (_,_,mtyp) -> treat_module_type id mtyp (* should do more ?*)
  in
  List.iter 
    (function  
      | Sig_value (id,_)
      | Sig_type (id, _, _) 
      | Sig_exception (id, _) ->
	add_internal_reference id
	  
      | Sig_module (id, mtyp,_) ->
	add_internal_reference id;
	treat_module_type id mtyp

      | Sig_modtype (id, mdecl) -> 
	add_internal_reference id;
	(match mdecl with
	    Modtype_abstract -> ()
	  | Modtype_manifest mtyp -> treat_module_type id mtyp
	)

      | Sig_class (id, _, _)
      | Sig_class_type (id, _, _) -> add_internal_reference id
    ) 
    sig_list      	  
 
(* TODO add support for references *)
let rec generate_text_element local elem =
  match elem with
    | Raw s -> <:html<$str:s$>>
    | Code s -> <:html<<code class="code">$str:s$</code>&>>
    | PreCode s -> <:html<<pre class="codepre"><code class="code">$str:s$</code></pre>&>>
    | Verbatim s -> <:html<<span class="verbatim">$str:s$</span>&>>
    | Style(sk, t) -> generate_style local sk t
    | List items -> <:html<<ul>$generate_list_items local items$</ul>&>>
    | Enum items -> <:html<<ol>$generate_list_items local items$</ol>&>>
    | Newline -> <:html<<br/>&>>(*should be : <:html<<p>&>>*)
    | Block text -> <:html<<blockquote>$generate_text local text$</blockquote>&>>
    | Title(n, lbl, t) -> generate_title n lbl (generate_text local t)
    | Ref(rk, s, t) -> (* ref check*)
      <:html<TODO reference : $str:s$>>
    | Special_ref _ -> <:html<TODO special ref>> (* raise (Failure "Not implemented") *)
    | Target _ -> <:html<TODO target>> (* raise (Failure "Not implemented") *)

and generate_text local text =
  List.fold_left 
    (fun acc elem -> <:html<$acc$$generate_text_element local elem$>>)
    Cow.Html.nil
    text

and generate_list_items local items =
  List.fold_left 
    (fun acc item -> <:html<$acc$<li>$generate_text local item$</li>&>>)
    Cow.Html.nil
    items

and generate_style local sk t = 
  let f elem = 
    match sk with
    | SK_bold -> <:html<<b>$elem$</b>&>>
    | SK_italic -> <:html<<i>$elem$</i>&>>
    | SK_emphasize -> <:html<<em>$elem$</em>&>>
    | SK_center -> <:html<<center>$elem$</center>&>>
    | SK_left -> <:html<<div align="left">$elem$</div>&>>
    | SK_right -> <:html<<div align="right">$elem$</div>&>>
    | SK_superscript -> <:html<<sup class="superscript">$elem$</sup>&>>
    | SK_subscript -> <:html<<sub class="subscript">$elem$</sub>&>>
    | SK_custom _ -> <:html<TODO custom>> 
  (*TODO raise (Failure "Not implemented: Custom styles")*)
  in
  f (generate_text local t)

and generate_title n lbl text =
  let sn = (string_of_int n) in
  let ftag id html = match n with
    | 1 -> <:html<<h1 id="$str:id$">$html$</h1>&>>
    | 2 -> <:html<<h2 id="$str:id$">$html$</h2>&>>
    | 3 -> <:html<<h3 id="$str:id$">$html$</h3>&>>
    | 4 -> <:html<<h4 id="$str:id$">$html$</h4>&>>
    | 5 -> <:html<<h5 id="$str:id$">$html$</h5>&>>
    | 6 -> <:html<<h6 id="$str:id$">$html$</h6>&>>
    | n when n > 6 -> 
      let clazz = "h"^sn in <:html<<div class="$str:clazz$">$html$</div>&>>
    | _ -> Cow.Html.nil in
  let id = match lbl with 
    | Some s ->  s
    | _ -> sn^"_"^"TITLE"
  in <:html<<br/>$ftag id text$<br/>&>>
  
let generate_authors local authors = 
  List.fold_left 
    (fun acc author -> <:html<$acc$<span class="author">$str:author$</span>&>>)
    <:html<<b>Author(s): </b>&>>
    authors

let generate_sees local sees = 
  let gen_see (sr, t) = 
    match sr with
    | See_url s -> 
      <:html< <a href="$str:s$">$generate_text local t$</a>&>> (*  class="url" *)
    | See_file s -> 
        <:html<<code class="code">$str:s$ </code>$generate_text local t$>>
    | See_doc s -> 
        <:html<<i class="doc">$str:s$ </i>$generate_text local t$>>
  in
  let elems = 
    List.fold_left 
      (fun acc see -> <:html< $acc$ <li>$gen_see see$</li>&>>)
      Cow.Html.nil
      sees in
  <:html<<b>See also</b> <ul>$elems$</ul>&>>

let generate_befores local befores = 
  let gen_before (s, t) =
    <:html<<b>Before $str:s$</b> $generate_text local t$>>
  in
    List.fold_left 
      (fun acc before -> <:html<$acc$$gen_before before$<br/>&>>)
      Cow.Html.nil
      befores

let generate_params local params = 
  let gen_param (s, t) =
    <:html<<div class="param_info"><code class="code">$str:s$</code> : $generate_text local t$</div>&>>
  in
    List.fold_left 
      (fun acc param -> <:html<$acc$$gen_param param$&>>)
      Cow.Html.nil
      params

let generate_raised local raised = 
  let gen_raised (s, t) =
    <:html<<code>$str:s$</code> $generate_text local t$<br/>&>>
  in
    List.fold_left 
      (fun acc raised -> <:html<$acc$$gen_raised raised$>>)
      Cow.Html.nil
      raised


(* TODO add support for custom tags *)
let generate_info local info = 
  let jinfo = 
    match info.i_desc with
	Some t -> generate_text local t
      | None -> Cow.Html.nil
  in
  let jinfo = 
    match info.i_authors with
      | [] -> jinfo
      | authors -> 
        <:html<$jinfo$<div class="authors">$generate_authors local authors$</div>&>>
  in
  let jinfo = 
    match info.i_version with
      | None -> jinfo
      | Some s -> 
        <:html<$jinfo$<div class="version">$str:s$</div>&>>
  in
  let jinfo = 
    match info.i_sees with
      | [] -> jinfo
      | sees -> 
	<:html<$jinfo$<div class="see">$generate_sees local sees$</div>&>>
  in
  let jinfo = 
    match info.i_since with
      | None -> jinfo
      | Some s -> 
        <:html<$jinfo$<b>Since</b> $str:s$&>>
  in
  let jinfo = 
    match info.i_before with
      | [] -> jinfo
      | befores -> 
        <:html<$jinfo$$generate_befores local befores$>>
  in
  let jinfo = 
    match info.i_deprecated with
      | None -> jinfo
      | Some t -> 
        <:html<$jinfo$<span class="warning">Deprecated.</span> $generate_text local t$<br/>&>>
  in
  let jinfo = 
    match info.i_params with
      | [] -> jinfo
      | params -> 
        <:html<$jinfo$<div class="parameters">$generate_params local params$</div>&>>
  in
  let jinfo = 
    match info.i_raised_exceptions with
      | [] -> jinfo
      | raised -> 
        <:html<$jinfo$<b>Raises</b> $generate_raised local raised$&>>
  in
  let jinfo = 
    match info.i_return_value with
      | None -> jinfo
      | Some t -> 
	<:html<$jinfo$<b>Returns</b> $generate_text local t$>>
  in
  Html_utils.make_info (Some jinfo)
    
let generate_info_opt local info =
  match info with
    | Some i -> Some (generate_info local i)
    | None -> None

let generate_info_opt2 local info after_info =
  let info = generate_info_opt local info in
  let after_info = generate_info_opt local after_info in
  match info, after_info with
    | None, None -> None
    | Some i, None -> Some i
    | None, Some i -> Some i
    | Some i1, Some i2 -> Some <:html<$i1$$i2$>> (* todo fix the inclusion of comments *)

let generate_typ local typ = 
  Gentyp.type_scheme local typ.ctyp_type

let generate_typ_param param = 
  let s =
    match param with
	Some {txt=s} -> "'" ^ s
      | None -> "_"
  in
  <:html<$str:s$>>

let generate_class_param param = 
  <:html<$str:"'" ^ param.txt$>>

let generate_variant_constructor local parent_name info (_, {txt=name; _}, args, _) =
  let args = List.map (generate_typ local) args in
  let info = generate_info_opt local info in
  Html_utils.make_variant_cell parent_name name args info

let generate_record_label local parent_name info (_, {txt=name; _}, mut, typ, _) =
  let mut = match mut with Mutable -> true | Immutable -> false in
  let label_type = generate_typ local typ in
  let info = generate_info_opt local info in
  Html_utils.make_record_label_cell parent_name name mut label_type info

(* In the future: give the Typedtree.module_type to substitute the signature with
   the destructive constraint *)
let generate_with_constraint local (path, _, cstr) =
  let path = generate_html_path local path in
  match cstr with
    | Twith_type td -> 
      let typ = 
        match td.typ_manifest with
            Some typ -> generate_typ local typ
          | None -> assert false
      in
      <:html<type $path$ = $typ$>>
    | Twith_typesubst td -> 
      let typ = 
        match td.typ_manifest with
            Some typ -> generate_typ local typ
          | None -> assert false
      in
      <:html<type $path$ := $typ$>>
    | Twith_module(p, _) -> 
      <:html<module $path$ = $generate_html_path local p$>>
    | Twith_modsubst(p, _) ->
      <:html<module $path$ = $generate_html_path local p$>>

let generate_variance = function
  | true, false -> `Positive
  | false, true -> `Negative
  | _, _ -> `None

(* Generate the body of a type declaration *)
let generate_type_kind local parent_name dtk tk =
  
  let infos = 
    match dtk with 
      | Some (Dtype_abstract) | None -> []
      | Some (Dtype_variant infos) | Some (Dtype_record infos) -> infos
  in
  
  match tk with
    | Ttype_abstract -> Cow.Html.nil

    | Ttype_variant cstrs ->
      let rec loop cstrs dcstrs acc =
	match cstrs with
          | ((_, {txt = name; _}, _, _) as cstr) :: rest ->
            let dcstrl, drest = 
              List.partition (fun (n, _) -> n = name) dcstrs 
            in
            let item =
              match dcstrl with
                | dcstr :: []  -> 
		  generate_variant_constructor local parent_name (snd dcstr) cstr 
		| [] -> generate_variant_constructor local parent_name None cstr
		| _ -> assert false
            in
            loop rest drest (item :: acc)	 
          | [] -> 
            if dcstrs <> [] then
	      Printf.eprintf "[Warning] generate_type_kind : Unbound documentation\n%!";
            List.rev acc
      in
      let items = loop cstrs infos [] in
      Html_utils.make_type_table items
  	
    | Ttype_record lbls ->
       let rec loop lbls dlbls acc =
        match lbls with
          | ((_, {txt = name}, _, _, _) as lbl) :: rest ->
            let dlbll, drest = 
              List.partition (fun (n, _) -> n = name) dlbls
            in
            let item =
              match dlbll with
                | dlbl :: _ -> 
		  generate_record_label local parent_name (snd dlbl) lbl
		| [] -> generate_record_label local parent_name None lbl
            in
            loop rest drest (item :: acc)
          | [] -> 
            if dlbls <> [] then
	      Printf.eprintf "[Warning] generate_type_kind : Unbound documentation\n%!";
	    let closing_brac = <:html<<td>}</td>&>> in
	    List.rev (closing_brac::acc)
       in
       let items = loop lbls infos [] in
       Html_utils.make_type_table items

(** Returns a signature and a path option in order to wrap the content *)
let rec generate_class_type local dclty clty =
  let rec loop local dclty clty args_acc =
    match dclty, clty.cltyp_desc with
      | (Some Dcty_constr|None), Tcty_constr(path, _, cor_list) ->
	let params = List.map (generate_typ local) cor_list in
	
	let path = get_path local ~is_class:true path in
	let html_path = path_to_html path in
	
	let args = 
	  Html.code ~cls:"type" (List.fold_left 
				   (fun acc typ -> <:html<$acc$$typ$ -> >>) 
				   Cow.Html.nil (List.rev args_acc)) in
	
	let params = 
	  Html_utils.html_of_type_class_param_list
	    params (List.map (fun _ -> `None) params) (* dummy variance list *)
	in

	let body = <:html<$args$$params$$html_path$>> in
	
	Ident (body, path)
	  
      | dclass_sig, Tcty_signature class_sig ->
	let fields : Cow.Html.t list = 
	  generate_class_type_fields local 
	    (match dclass_sig with Some Dcty_signature cl -> Some cl | _ -> None)
	    (* The fields are apparently reversed... *)
	    (List.rev class_sig.csig_fields) in
	
	let args = match args_acc with
	  | [] -> Cow.Html.nil 
	  | l -> Html.code ~cls:"type"
	    (List.fold_left (fun acc typ -> <:html<$acc$$typ$ -> >>) Cow.Html.nil l)
	in 
	
	let body = 
          let objcode = Html.code ~cls:"code" (Html.html_of_string "object") in
          let endcode = Html.code ~cls:"code" (Html.html_of_string "end") in
	  <:html<$args$$objcode$ .. $endcode$>> in
	  
	  Sig (<:html<$body$>>, Html_utils.create_class_signature_content fields)
		     
      | dclass_type, Tcty_fun (_, core_type, sub_class_type) ->
	let arg = generate_typ local core_type in
	let sub_dclass_type = 
	  match dclass_type with 
	    | Some (Dcty_fun dcty) -> Some dcty 
	    | None -> None 
	    | _ -> raise (Failure "generate_class_type : Mismatch") in
	
	loop local sub_dclass_type sub_class_type (arg::args_acc)

      | _, _ -> assert false
  in
  loop local dclty clty []

(* TODO : Double check this function *)
and generate_class_type_fields local dclsigl tclsigl =
    let open Html_utils in
    let process_class_field local tfield = 
      match tfield.ctf_desc with
	| Tctf_inher ctyp -> 
	  let class_result = generate_class_type local None ctyp in
	    
	  begin
	    match class_result with
	      | Ident (s, p) ->
		let signature = Html.pretrack 1 <:html<$keyword "inherit"$ $s$>> in
		create_class_container "_inherit_field" signature Cow.Html.nil (Some p)
	      | Sig (s,c) -> 
		let signature = Html.pretrack 2 <:html<$keyword "inherit"$ $s$>> in
		create_class_container "_inherit_field" signature c None
	  end

	| Tctf_val (name, mut_f, virt_f, co_typ) ->  
	  let typ = generate_typ local co_typ in
	  let mut = match mut_f with | Mutable -> true | Immutable -> false in
	  let virt = match virt_f with | Virtual -> true | Concrete -> false in
	  let label = keyword "val" in
	  let label = 
	    if virt then <:html<$label$ $keyword "virtual"$>> else label in
	  let label = 
	    if mut then <:html<$label$ $keyword "mutable"$>> else label in
	  let label = generate_mark Opam_doc_config.Value name 
	    <:html<$label$ $str:name$>> in
          let cd = Html.code ~cls:"code" typ in
	  Html.pretrack 3 <:html<$label$ : $cd$>>
	| Tctf_meth (name, priv_f, co_typ) -> 
	  let typ = generate_typ local co_typ in
	  let priv = match priv_f with Private -> true | Public -> false in
	  
	  let label = keyword "method" in
	  let label = 
	    if priv then <:html<$label$ $keyword "private"$>> else label in
	  let label = generate_mark Opam_doc_config.Method
	    name <:html<$label$ $str:name$>> in
          let cd = Html.code ~cls:"code" typ in
	  Html.pretrack 4 <:html<$label$ : $cd$>>
	| Tctf_virt (name, priv_f, co_typ) -> 
	  let typ = generate_typ local co_typ in
	  let priv = match priv_f with Private -> true | Public -> false in
	  
	  let label = keyword "method" in
	  let label = <:html<$label$ $keyword "virtual"$>> in
	  let label = 
	    if priv then <:html<$label$ $keyword "private"$>> else label in
	  let label = generate_mark Opam_doc_config.Method
	    name <:html<$label$ $str:name$>> in
          let cd = Html.code ~cls:"code" typ in
	  Html.pretrack 5 <:html<$label$ : $cd$>>
	| Tctf_cstr (co_typ1, co_typ2) ->
	  let jtyp1 = generate_typ local co_typ1 in
	  let jtyp2 = generate_typ local co_typ2 in
	  let label = <:html<$jtyp1$ = $jtyp2$>> in
          let cd = Html.code ~cls:"type" label in
	  let label = <:html<$cd$>> in
	  Html.pretrack 6 <:html<$keyword "constraint"$ $label$>>	  
    in
    
    let generate_class_type_fields_with_doctree local dclsigl tclsigl =
      let rec loop (acc: Cow.Html.t list) local dclsigl tclsigl is_stopped =
    	match dclsigl, tclsigl with
	  | [], r::l -> 
	    Printf.eprintf 
	      "generate_class_type_fields mismatch -- processing without doc\n%!";
	    List.rev acc @ List.map (process_class_field local) (r::l)
	  | { dctf_desc=Dctf_comment
	    ; dctf_info=i1
	    ; dctf_after_info=i2}::r, r2 -> 
	    if is_stopped then
	      loop acc local r r2 is_stopped
	    else 
	      let info = Html_utils.make_info (generate_info_opt2 local i1 i2) in
	      loop (info::acc) local r r2 is_stopped
	  | { dctf_desc=Dctf_stop; _}::r, r2 -> loop acc local r r2 (not is_stopped)
	  | d::r, t::r2 ->
	    begin
	      match d.dctf_desc, t.ctf_desc with
		| Dctf_inher dctyp, Tctf_inher ctyp -> 
		  let item = 
		    let class_result = generate_class_type local None ctyp in
		    begin
		      match class_result with
			| Ident (s, p) ->
			  let signature = Html.pretrack 7 <:html<$keyword "inherit"$ $s$>> in
			  create_class_container "_inherit_field" signature Cow.Html.nil (Some p)
			| Sig (s,c) -> 
			  let signature = Html.pretrack 8 <:html<$keyword "inherit"$ $s$>> in
			  create_class_container "_inherit_field" signature c None
		    end
		      
		  in
		  if is_stopped then
		    loop (item::acc) local r r2 is_stopped
		  else 
		    let info = Html_utils.make_info
		      (generate_info_opt2 local d.dctf_info d.dctf_after_info) in
		    loop (info::item::acc) local r r2 is_stopped
		| Dctf_val _, Tctf_val _ 
		| Dctf_meth _, Tctf_meth _ 
		| Dctf_meth _, Tctf_virt _ 
		| Dctf_cstr, Tctf_cstr _ -> 
		  let item = process_class_field local t in
		  if is_stopped then
		    loop (item::acc) local r r2 is_stopped
		  else 
		    let info =  Html_utils.make_info
		      (generate_info_opt2 local d.dctf_info d.dctf_after_info) in
		    loop (info::item::acc) local r r2 is_stopped
		| _,_ -> 
		  Printf.eprintf 
		    "generate_class_type_fields mismatch -- processing without doc\n%!";
		  List.rev acc @ List.map (process_class_field local) (t::r2)
	    end
	  | _, [] -> List.rev acc
	    
      in
      loop [] local dclsigl tclsigl false
    in
    
    match dclsigl with
      | Some dclsigl -> generate_class_type_fields_with_doctree local dclsigl tclsigl
      | None ->  List.map (process_class_field local) tclsigl

(** Returns a signature and a path option in order to wrap the content *)
let rec generate_class_expr local dclexpr ci_expr = 
  (* Gros bugs en vue avec args_acc *)
  let rec loop local dclexpr ci_expr args_acc =
    match dclexpr, ci_expr.cl_desc with
      | dclass_struct, Tcl_structure {cstr_fields=fields; _} -> 
	let fields = generate_class_fields local
	  (match dclass_struct with Some (Dcl_structure str) -> Some str | _ -> None)
	  (* don't reverse the fields this time? :l *)
	  fields in
	
	let args = match args_acc with
	  | [] -> Cow.Html.nil 
	  | l -> Html.code ~cls:"type"
	    (List.fold_left (fun acc typ -> <:html<$acc$$typ$ -> >>) Cow.Html.nil l)
	in
	let signature = 
	  let open Html_utils in 
          let objcode = Html.code ~cls:"code" (Html.html_of_string "object") in
          let endcode = Html.code ~cls:"code" (Html.html_of_string "end") in
	      <:html<$args$$objcode$ .. $endcode$>> in
		   
	  Sig (signature, <:html<$Html_utils.create_class_signature_content fields$>>)
	  
      | dclass_expr, Tcl_fun (_, pattern, _, class_expr, _) -> 
	let arg = Gentyp.type_scheme local pattern.pat_type in
	
	loop local 
	  (match dclass_expr with Some (Dcl_fun e) -> Some e | _ -> None)
	  class_expr (arg::args_acc)
	  
      | dclass_apply, Tcl_apply (class_expr, list) -> 
	(* Not sure... (neither does ocamldoc) *)
	loop local 
	  (match dclass_apply with Some (Dcl_apply e) -> Some e | _ -> None)
	  class_expr args_acc  

      | dclass_let, Tcl_let (_, _, _, class_expr) -> 
	(* just process through *)
	loop local
	  (match dclass_let with Some (Dcl_let e) -> Some e | _ -> None)
	  class_expr args_acc
	  
      | (Some Dcl_constr | None), Tcl_constraint (class_expr, _, _, _, _) -> 
	(* Weird matching: to double check *)
	let params, path = 
	  match class_expr.cl_desc with 
	    | Tcl_ident (path, _, co_typ_list) -> 
	      let params = List.map 
		(generate_typ local)
		co_typ_list in
	      let path = get_path local ~is_class:true path in
	      params, path
	    | _ -> assert false
	in
	  
	let html_path = path_to_html path in
	let args = 
	  Html.code ~cls:"type" (List.fold_left 
				   (fun acc typ -> <:html<$acc$$typ$ -> >>) 
				   Cow.Html.nil (List.rev args_acc)) in
	let params = 
	  Html_utils.html_of_type_class_param_list
	    params (List.map (fun _ -> `None) params) (* dummy variance list *)
	in

	Ident (<:html<$args$$params$$html_path$>>, path)

      | dclass_constraint, Tcl_constraint (class_expr, Some ctyp, _, _, _) ->

	let cte = loop local 
	  (match dclass_constraint with Some (Dcl_constraint (e,_)) -> Some e | _ -> None)
	  class_expr [] in
	let ctyp = generate_class_type local 
	  (match dclass_constraint with Some (Dcl_constraint (_,t)) -> Some t | _ -> None)
	  ctyp in
	
	let ctyp_sig = match ctyp with | Ident (s, _) | Sig (s, _) -> s in
	
	let args = match args_acc with
	  | [] -> Cow.Html.nil 
	  | l -> Html.code ~cls:"type"
	    (List.fold_left (fun acc typ -> <:html<$acc$$typ$ -> >>) Cow.Html.nil l)
	in 
	begin
	  match cte with
	    | Ident (s, p) -> Ident (<:html<$args$( $s$ : $ctyp_sig$ )>>, p)
	    | Sig (s, c) -> Sig (<:html<$args$( $s$ : $ctyp_sig$ )>>, c)
	end	  
      | _,_ -> raise (Failure "generate_class_expr: Mismatch")
  in
  loop local dclexpr ci_expr []

and generate_class_fields local (dclexpr : Doctree.class_field list option) tclexpr = 
      let extract_type_and_virtual_from_kind = function
    (* Type method = < obj type; .. > -> real type 
       small hack to remove the first part
    *)
    | Tcfk_virtual co_typ ->
      let exp_type = co_typ.ctyp_type in
      (match exp_type.desc with
	| Tarrow (_, _, ty2, _) -> Gentyp.type_scheme local ty2, false
	| _ -> Gentyp.type_scheme local exp_type, false
      )      
    | Tcfk_concrete expr -> 
      (match expr.exp_type.desc with
	| Tarrow (_, _, ty2, _) -> Gentyp.type_scheme local ty2, false
	| _ -> Gentyp.type_scheme local expr.exp_type, false
      )
      in
      
    let process_class_field local tfield = 
      let open Html_utils in
	  match tfield.cf_desc with
	    | Tcf_inher (_, cexpr, _, _, _) -> 
	      let class_result = generate_class_expr local None cexpr in
	      begin
		match class_result with
		  | Ident (s, p) -> 
		    let signature = Html.pretrack 9 <:html<$keyword "inherit"$ $s$>> in
		    create_class_container "_inherit_field" signature Cow.Html.nil (Some p)
		  | Sig (s,c) -> 
		    let signature = Html.pretrack 10 <:html<$keyword "inherit"$ $s$>> in
		    create_class_container "_inherit_field" signature c None
	      end

	    | Tcf_val (name, _, mut_f, _, cl_f_kind, _) ->
	      
	      let typ,virt = extract_type_and_virtual_from_kind cl_f_kind in
	      let mut = match mut_f with | Mutable -> true | Immutable -> false in
	      
	      let label = keyword "val" in
	      let label = 
		if virt then <:html<$label$ $keyword "virtual"$>> else label in
	      let label = 
		if mut then <:html<$label$ $keyword "mutable"$>> else label in
	      let label = generate_mark Opam_doc_config.Attribute name 
		<:html<$label$ $str:name$>> in
              let cd = Html.code ~cls:"code" typ in
	      Html.pretrack 11 <:html<$label$ : $cd$>>		   
	    
	    | Tcf_meth (name, _, priv_f, cl_f_kind, ovr_b) ->
	      let typ, virt = extract_type_and_virtual_from_kind cl_f_kind in
	      let priv = match priv_f with Private -> true | Public -> false in
	      	      
	      let label = keyword "method" in
	      let label = 
		if virt then <:html<$label$ $keyword "virtual"$>> else label in
	      let label = 
		if priv then <:html<$label$ $keyword "private"$>> else label in
	      let label = generate_mark Opam_doc_config.Method 
		name <:html<$label$ $str:name$>> in
              let cd = Html.code ~cls:"code" typ in
	      Html.pretrack 12 <:html<$label$ : $cd$>>
	    | Tcf_constr (co_typ1, co_typ2) ->
	      let jtyp1 = generate_typ local co_typ1 in
	      let jtyp2 = generate_typ local co_typ2 in
	      let label = <:html<$jtyp1$ = $jtyp2$>> in
              let cd = Html.code ~cls:"type" label in
	      let label = <:html<$cd$>> in
	      Html.pretrack 13 <:html<$keyword "constraint"$ $label$>>	  
	    | Tcf_init _ -> Cow.Html.nil
      in
      
      let generate_class_type_fields_with_doctree local dclexpr tclexpr =
	let rec loop (acc: Cow.Html.t list) local dclsigl tclsigl is_stopped =
    	  match dclsigl, tclsigl with
	    | [], r::l -> 
	      Printf.eprintf "generate_class_expr_fields mismatch -- processing without doc\n%!";
	      List.rev acc @ List.map (process_class_field local) (r::l)
	    | { dcf_desc=Dcf_comment
	      ; dcf_info=i1 }::r, r2 -> 
	      if is_stopped then
		loop acc local r r2 is_stopped
	      else 
		let info = Html_utils.make_info (generate_info_opt local i1) in
		loop (info::acc) local r r2 is_stopped
	    | { dcf_desc=Dcf_stop; _}::r, r2 -> loop acc local r r2 (not is_stopped)
	    | d::r, t::r2 ->
	      begin
		match d.dcf_desc, t.cf_desc with
		  | Dcf_inher dctyp, Tcf_inher (_, cexpr, _, _, _) -> 
		    let item =
		      let open Html_utils in
			  let class_result = generate_class_expr local None cexpr in
			  begin
			    match class_result with
			      | Ident (s, p) ->
				let signature = Html.pretrack 14 <:html<$keyword "inherit"$ $s$>> in
				create_class_container "_inherit_field" signature Cow.Html.nil (Some p)
			      | Sig (s,c) -> 
				let signature = Html.pretrack 15 <:html<$keyword "inherit"$ $s$>> in
				create_class_container "_inherit_field" signature c None
			  end
		      in
		      if is_stopped then
			loop (item::acc) local r r2 is_stopped
		      else 
			let info = Html_utils.make_info
			  (generate_info_opt local d.dcf_info) in
			loop (info::item::acc) local r r2 is_stopped
		  | Dcf_val _, Tcf_val _ 
		  | Dcf_meth _, Tcf_meth _ 
		  | Dcf_constr, Tcf_constr _ -> 
		    let item = process_class_field local t in
		    if is_stopped then
		      loop (item::acc) local r r2 is_stopped
		    else 
		      let info =  Html_utils.make_info
			(generate_info_opt local d.dcf_info) in
		      loop (info::item::acc) local r r2 is_stopped
		  | _,_ -> 
		    Printf.eprintf "generate_class_expr_fields mismatch -- processing without doc\n%!";
		    List.rev acc @ List.map (process_class_field local) (t::r2)
	      end
	    | _, [] -> List.rev acc
	      
	in
	loop [] local dclexpr tclexpr false
      in
      
      match dclexpr with
	| Some dclexpr -> generate_class_type_fields_with_doctree local dclexpr tclexpr
	| None ->  List.map (process_class_field local) tclexpr


let rec generate_module_type local dmty mty = 
  let open Html_utils in
  match dmty, mty.mty_desc with
    | (Some Dmty_ident | None), Tmty_ident(path, _) -> 
      let real_path = get_path local path in
      let path = path_to_html real_path in
      let cd = Html.code ~cls:"code" path in
      Ident  (<:html<$cd$&>>, real_path)

    | Some (Dmty_signature dsg), Tmty_signature sg ->
      let sg_items = generate_signature_item_list local (Some dsg) sg.sig_items in
      let signature = 
        let sigcode = Html.code ~cls:"code" (Html.html_of_string "sig") in
        let endcode = Html.code ~cls:"code" (Html.html_of_string "end") in
	<:html<$sigcode$ .. $endcode$>>
      in
      let module_content = create_module_signature_content sg_items in
      Sig (signature, module_content)
    | None, Tmty_signature sg ->
      let sg_items = generate_signature_item_list local None sg.sig_items in
      let signature = 
        let sigcode = Html.code ~cls:"code" (Html.html_of_string "sig") in
        let endcode = Html.code ~cls:"code" (Html.html_of_string "end") in
	<:html<$sigcode$ .. $endcode$>>
      in
      let module_content = create_module_signature_content sg_items in
      Sig (signature, module_content)
	
    | Some (Dmty_functor(darg, dbase)), Tmty_functor(id, _, arg, base) ->
      let arg = match generate_module_type local (Some darg) arg with
	| Sig (s, _) | Ident (s, _) -> s in
      let base = generate_module_type local (Some dbase) base in
      
      let cd = Html.code ~cls:"code" (Html.html_of_string "functor (") in
      let label = <:html<$cd$>> in
      let cd = Html.code ~cls:"code" (Html.html_of_string id.Ident.name) in
      let label = <:html<$label$$cd$>> in
      let label = <:html<$label$<code class="code"> : </code>&>> in
      let cd = Html.code ~cls:"code" (Html.html_of_string ") -> ") in
      let label = <:html<$label$$arg$$cd$>> in
      
      begin
	match base with 
	  | Ident (msig, p) -> 
	    Ident (<:html<<div class="sig_block">$label$$msig$</div>&>>, p)
	  | Sig (msig, content) -> 
	    Sig (<:html<<div class="sig_block">$label$$msig$</div>&>>, content)
      end
    | None, Tmty_functor(id, _, arg, base) ->
      let arg = match generate_module_type local None arg with
	| Sig (s, _) | Ident (s, _) -> s in
      let base = generate_module_type local None base in
      
      let cd = Html.code ~cls:"code" (Html.html_of_string "functor (") in
      let label = <:html<$cd$>> in
      let cd = Html.code ~cls:"code" (Html.html_of_string id.Ident.name) in
      let label = <:html<$label$$cd$>> in
      let label = <:html<$label$<code class="code"> : </code>&>> in
      let cd = Html.code ~cls:"code" (Html.html_of_string ") -> ") in
      let label = <:html<$label$$arg$$cd$>> in
      
      begin
	match base with 
	  | Ident (msig, p) -> 
	    Ident (<:html<<div class="sig_block">$label$$msig$</div>&>>, p)
	  | Sig (msig, content) -> 
	    Sig (<:html<<div class="sig_block">$label$$msig$</div>&>>, content)
      end
	
    | Some (Dmty_with dbase), Tmty_with(base, cnstrs) ->
      let base = generate_module_type local (Some dbase) base in
      let cnstrs = List.map (generate_with_constraint local) cnstrs in

      let constraints = Html.concat ~sep:" and " cnstrs in
      let signature = <:html<with $constraints$>> in
	begin
	  match base with 
	    | Ident (msig, p) -> Ident (<:html<$msig$ $signature$>>, p)
	    | Sig (msig, content) -> Sig (<:html<$msig$ $signature$>>, content)
	end
	
    | None, Tmty_with(base, cnstrs) ->
      let base = generate_module_type local None base in
      let cnstrs = List.map (generate_with_constraint local) cnstrs in
      let constraints = Html.concat ~sep:" and " cnstrs in
      let signature = <:html<with $constraints$>> in
      begin
	match base with 
	  | Ident (msig, p) -> Ident (<:html<$msig$$signature$>>, p)
	  | Sig (msig, content) -> Sig (<:html<$msig$$signature$>>, content)
      end
	
    | Some (Dmty_typeof dexpr), Tmty_typeof mexpr ->
      let base = generate_module_expr local (Some dexpr) mexpr in
      
      begin
	match base with 
	  | Ident (msig, p) -> Ident (<:html<module type of $msig$>>, p)
	  | Sig (msig, content) -> Sig (<:html<module type of $msig$>>, content)
      end
      
    | None, Tmty_typeof mexpr ->
      let base = generate_module_expr local None mexpr in

      begin
	match base with 
	  | Ident (msig, p) -> Ident (<:html<module type of $msig$>>, p)
	  | Sig (msig, content) -> Sig (<:html<module type of $msig$>>, content)
      end
      
    | _, _ -> raise (Failure "generate_module_type: Mismatch")
      
and generate_module_expr local dmexpr tmexpr =
  let open Html_utils in
  match dmexpr, tmexpr.mod_desc with
    | (Some Dmod_ident | None), Tmod_ident(path, _) -> 
      let real_path = get_path local path in
      let path = path_to_html real_path in
      let cd = Html.code ~cls:"code" path in
      Ident (<:html<$cd$&>>, real_path)

    | Some (Dmod_structure dstr), Tmod_structure str ->
      let str_items = generate_structure_item_list local (Some dstr) str.str_items in
      let signature = 
        let sigcode = Html.code ~cls:"code" (Html.html_of_string "sig") in
        let endcode = Html.code ~cls:"code" (Html.html_of_string "end") in
	<:html<$sigcode$ .. $endcode$>>
      in
      let module_content = create_module_signature_content str_items in
      Sig  (signature, module_content)

    | None, Tmod_structure str ->
      let str_items = generate_structure_item_list local None str.str_items in
      let signature = 
        let sigcode = Html.code ~cls:"code" (Html.html_of_string "sig") in
        let endcode = Html.code ~cls:"code" (Html.html_of_string "end") in
	<:html<$sigcode$ .. $endcode$>>
      in
      let module_content = create_module_signature_content str_items in
      Sig  (signature, module_content)
	
    | Some (Dmod_functor(darg, dbase)), Tmod_functor(id, _, arg, base) ->
      let arg = match generate_module_type local (Some darg) arg with 
	| Sig (s, _) | Ident (s, _) -> s in
      let base = generate_module_expr local (Some dbase) base in
      
      let cd = Html.code ~cls:"code" (Html.html_of_string "functor (") in
      let label = <:html<$cd$>> in
      let cd = Html.code ~cls:"code" (Html.html_of_string id.Ident.name) in
      let label = <:html<$label$$cd$>> in
      let label = <:html<$label$<code class="code"> : </code>&>> in
      let cd = Html.code ~cls:"code" (Html.html_of_string ") -> ") in
      let label = <:html<$label$$arg$$cd$>> in
      
      begin
	match base with 
	  | Ident (msig, p) -> 
	    Ident (<:html<<div class="sig_block">$label$$msig$</div>&>>, p)
	  | Sig (msig, content) -> 
	    Sig (<:html<<div class="sig_block">$label$$msig$</div>&>>, content)
      end

    | None, Tmod_functor(id, _, arg, base) ->
      let arg = match generate_module_type local None arg with 
	| Sig (s, _) | Ident (s, _) -> s in
      let base = generate_module_expr local None base in
      
      let cd = Html.code ~cls:"code" (Html.html_of_string "functor (") in
      let label = <:html<$cd$>> in
      let cd = Html.code ~cls:"code" (Html.html_of_string id.Ident.name) in
      let label = <:html<$label$$cd$>> in
      let label = <:html<$label$<code class="code"> : </code>&>> in
      let cd = Html.code ~cls:"code" (Html.html_of_string ") -> ") in
      let label = <:html<$label$$arg$$cd$>> in
      
      begin
	match base with 
	  | Ident (msig, p) -> 
	    Ident (<:html<<div class="sig_block">$label$$msig$</div>&>>, p)
	  | Sig (msig, content) -> 
	    Sig (<:html<<div class="sig_block">$label$$msig$</div>&>>, content)
      end
	
    | Some (Dmod_apply (dmexpr1, dmexpr2)), Tmod_apply (tmexpr1, tmexpr2, _) ->
      let base = generate_module_expr local (Some dmexpr1) tmexpr1 in
      let arg = match generate_module_expr local (Some dmexpr2) tmexpr2 with 
	| Sig (s, _) | Ident (s, _) -> s in
      begin
	match base with 
	  | Ident (msig, p) -> 
	    Ident (<:html<$msig$($arg$)>>, p)
	  | Sig (msig, content) -> 
	    Sig (<:html<$msig$($arg$)>>, content)
      end
    | None, Tmod_apply (tmexpr1, tmexpr2, _) -> 
      let base = generate_module_expr local None tmexpr1 in
      let arg = match generate_module_expr local None tmexpr2 with 
	| Sig (s, _) | Ident (s, _) -> s in
      begin      
	match base with 
	  | Ident (msig, p) -> 
	    Ident (<:html<$msig$($arg$)>>, p)
	  | Sig (msig, content) -> 
	    Sig (<:html<$msig$($arg$)>>, content)
      end
    | dmod_constraint_opt, Tmod_constraint(_, _, Tmodtype_explicit tmty, _) ->

	generate_module_type local
	  (match dmod_constraint_opt with
	    | Some (Dmod_constraint (_, dmty)) -> Some dmty
	    | None | _ -> None)
	  tmty

    | dmod_constraint_opt, Tmod_constraint(tmexpr, _, Tmodtype_implicit, _) ->
      
      generate_module_expr local
	(match dmod_constraint_opt with
	  | Some (Dmod_constraint (dmexpr, _)) -> Some dmexpr
	  |  _ -> None)
	tmexpr

    | (Some Dmod_unpack | None), Tmod_unpack(tmexpr, tmty) -> 
      (* Not sure what to do with an unpack module (first class module) *)
      begin
	match tmty with 
	  | Mty_ident path -> 
	    let real_path = get_path local path in
	    let path = path_to_html real_path in
            let cd = Html.code ~cls:"code" path in
	    Ident (<:html<$cd$&>>, real_path)
	  | _ -> 	  
	    Printf.eprintf "generate_module_expr: unpack mismatch\n%!";
	    assert false
      end
    | _, _ -> raise (Failure "generate_module_type: Mismatch")
   
(* Discards open, expands module_rec and process signature items  *)
and generate_signature_item_list local dsig_items tsig_items =
    let rec loop_with_doctree ditems items acc is_stopped =
      match ditems, items with
        | [], _ -> List.rev acc
	| {dsig_desc = Dsig_comment; dsig_info=info; _} :: drest, _ ->
          if is_stopped then
	    loop_with_doctree drest items acc is_stopped
	  else
	    let comment = Html_utils.make_info (generate_info_opt local info) in
            loop_with_doctree drest items (comment :: acc) is_stopped
	| {dsig_desc = Dsig_stop; _} :: drest, _ ->
	  loop_with_doctree drest items acc (not is_stopped)
	| {dsig_desc = Dsig_open; _} :: drest, {sig_desc = Tsig_open _; _} :: rest ->
	  (* We discard the 'open' elements *)
          loop_with_doctree drest rest acc is_stopped
	| ({dsig_desc = Dsig_type _; _} as ditem) :: drest, 
          ({sig_desc = Tsig_type (tnext :: trest); _} as item) :: rest -> 
          let item = {item with sig_desc = Tsig_type [tnext]}
          and rest = 
            match trest with
		[] -> rest
              | _ -> {item with sig_desc = Tsig_type trest} :: rest
          in
          let jitem = generate_signature_item local (Some ditem) item in
          loop_with_doctree drest rest (jitem :: acc) is_stopped
	| ({dsig_desc = Dsig_recmodule _; _} as ditem) :: drest, 
          ({sig_desc = Tsig_recmodule (mnext :: mrest)} as item) :: rest -> 
          let item = {item with sig_desc = Tsig_recmodule [mnext]} 
          and rest = 
            match mrest with
		[] -> rest
              | _ -> {item with sig_desc = Tsig_recmodule mrest} :: rest
          in
          let jitem = generate_signature_item local (Some ditem) item in
          loop_with_doctree drest rest (jitem :: acc) is_stopped
	| ({dsig_desc = Dsig_class _; _} as ditem) :: drest, 
          ({sig_desc = Tsig_class (cnext :: crest)} as item) :: rest -> 
          let item = {item with sig_desc = Tsig_class [cnext]} 
          and rest =
            match crest with
		[] -> rest
              | _ -> {item with sig_desc = Tsig_class crest} :: rest
          in
          let jitem = generate_signature_item local (Some ditem) item in
          loop_with_doctree drest rest (jitem :: acc) is_stopped
	| ({dsig_desc = Dsig_class_type _; _} as ditem) :: drest, 
          ({sig_desc = Tsig_class_type (cnext :: crest)} as item) :: rest -> 
          let item = {item with sig_desc = Tsig_class_type [cnext]}
          and rest = 
            match crest with
		[] -> rest
              | _ -> {item with sig_desc = Tsig_class_type crest} :: rest
          in
          let jitem = generate_signature_item local (Some ditem) item in
          loop_with_doctree drest rest (jitem :: acc) is_stopped
	| ditem :: drest, item :: rest ->
          let jitem = generate_signature_item local (Some ditem) item in
          loop_with_doctree drest rest (jitem :: acc) is_stopped
	| _, _ -> 
	  (* raise (Failure "generate_signature_item_list: Mismatch") *)
	  List.rev acc
    in
  
    let rec loop_without_doctree items acc =
      match items with
        | [] -> List.rev acc
	| {sig_desc = Tsig_open _} :: rest ->
	  (* We discard the 'open' elements *)
          loop_without_doctree rest acc
	| ({sig_desc = Tsig_type (tnext :: trest); _} as item) :: rest -> 
          let item = {item with sig_desc = Tsig_type [tnext]}
          and rest = 
            match trest with
		[] -> rest
              | _ -> {item with sig_desc = Tsig_type trest} :: rest
          in
          let jitem = generate_signature_item local None item in
          loop_without_doctree rest (jitem :: acc)
	| ({sig_desc = Tsig_recmodule (mnext :: mrest); _} as item) :: rest -> 
          let item = {item with sig_desc = Tsig_recmodule [mnext]} 
          and rest = 
            match mrest with
		[] -> rest
              | _ -> {item with sig_desc = Tsig_recmodule mrest} :: rest
          in
          let jitem = generate_signature_item local None item in
          loop_without_doctree rest (jitem :: acc)
	|({sig_desc = Tsig_class (cnext :: crest); _} as item) :: rest -> 
          let item = {item with sig_desc = Tsig_class [cnext]} 
          and rest =
            match crest with
		[] -> rest
              | _ -> {item with sig_desc = Tsig_class crest} :: rest
          in
          let jitem = generate_signature_item local None item in
          loop_without_doctree rest (jitem :: acc)
	| ({sig_desc = Tsig_class_type (cnext :: crest); _} as item) :: rest -> 
          let item = {item with sig_desc = Tsig_class_type [cnext]}
          and rest = 
            match crest with
		[] -> rest
              | _ -> {item with sig_desc = Tsig_class_type crest} :: rest
          in
          let jitem = generate_signature_item local None item in
          loop_without_doctree rest (jitem :: acc) 
	| item :: rest ->
          let jitem = generate_signature_item local None item in
          loop_without_doctree rest (jitem :: acc)
    in
    match dsig_items with 
      | Some ditems -> loop_with_doctree ditems tsig_items [] false
      | None -> loop_without_doctree tsig_items []

(* Mirror function for structure items *)      
and generate_structure_item_list local (dstr_items : Doctree.structure_item list option) 
      tstr_items : Cow.Html.t list =

    let rec loop_with_doctree ditems items acc is_stopped =
      match ditems, items with
        | [], _ -> List.rev acc
	| {dstr_desc = Dstr_comment; dstr_info=info; _} :: drest, _ ->
          if is_stopped then
	    loop_with_doctree drest items acc is_stopped
	  else
	    let comment = Html_utils.make_info (generate_info_opt local info) in
            loop_with_doctree drest items (comment :: acc) is_stopped
	| {dstr_desc = Dstr_stop; _} :: drest, _ ->
	  loop_with_doctree drest items acc (not is_stopped)
	| {dstr_desc = Dstr_open; _} :: drest, {str_desc = Tstr_open _; _} :: rest ->
	  (* We discard the 'open' elements *)
          loop_with_doctree drest rest acc is_stopped
	| ({dstr_desc = Dstr_type _; _} as ditem) :: drest, 
          ({str_desc = Tstr_type (tnext :: trest); _} as item) :: rest -> 
          let item = {item with str_desc = Tstr_type [tnext]}
          and rest = 
            match trest with
		[] -> rest
              | _ -> {item with str_desc = Tstr_type trest} :: rest
          in
          let jitem = generate_structure_item local (Some ditem) item in
          loop_with_doctree drest rest (jitem :: acc) is_stopped
	| ({dstr_desc = Dstr_recmodule _; _} as ditem) :: drest, 
          ({str_desc = Tstr_recmodule (mnext :: mrest)} as item) :: rest -> 
          let item = {item with str_desc = Tstr_recmodule [mnext]} 
          and rest = 
            match mrest with
		[] -> rest
              | _ -> {item with str_desc = Tstr_recmodule mrest} :: rest
          in
          let jitem = generate_structure_item local (Some ditem) item in
          loop_with_doctree drest rest (jitem :: acc) is_stopped
	| ({dstr_desc = Dstr_class _; _} as ditem) :: drest, 
          ({str_desc = Tstr_class (cnext :: crest)} as item) :: rest -> 
          let item = {item with str_desc = Tstr_class [cnext]} 
          and rest =
            match crest with
		[] -> rest
              | _ -> {item with str_desc = Tstr_class crest} :: rest
          in
          let jitem = generate_structure_item local (Some ditem) item in
          loop_with_doctree drest rest (jitem :: acc) is_stopped
	| ({dstr_desc = Dstr_class_type _; _} as ditem) :: drest, 
          ({str_desc = Tstr_class_type (cnext :: crest)} as item) :: rest -> 
          let item = {item with str_desc = Tstr_class_type [cnext]}
          and rest = 
            match crest with
		[] -> rest
              | _ -> {item with str_desc = Tstr_class_type crest} :: rest
          in
          let jitem = generate_structure_item local (Some ditem) item in
          loop_with_doctree drest rest (jitem :: acc) is_stopped

	(* split the values' pattern *)
	| ({dstr_desc = Dstr_value _; _} as ditem) :: drest, 
          ({str_desc = Tstr_value (rflags, (cnext :: crest))} as item) :: rest -> 
	  let item = {item with str_desc = Tstr_value (rflags, [cnext])}
          and rest = 
            match crest with
	      | [] -> rest
              | _ -> {item with str_desc = Tstr_value (rflags, crest)} :: rest
          in
          let jitem = generate_structure_item local (Some ditem) item in
          loop_with_doctree drest rest (jitem :: acc) is_stopped
	  
	| ditem :: drest, item :: rest ->
          let jitem = generate_structure_item local (Some ditem) item in
          loop_with_doctree drest rest (jitem :: acc) is_stopped
	| _, _ -> 
	  (* raise (Failure "generate_structure_item_list: Mismatch") *)
	  List.rev acc
    
    in
    
    let rec loop_without_doctree items acc =
      match items with
        | [] -> List.rev acc
	| {str_desc = Tstr_open _} :: rest ->
	  (* We discard the 'open' elements *)
          loop_without_doctree rest acc
	| ({str_desc = Tstr_type (tnext :: trest); _} as item) :: rest -> 
          let item = {item with str_desc = Tstr_type [tnext]}
          and rest = 
            match trest with
		[] -> rest
              | _ -> {item with str_desc = Tstr_type trest} :: rest
          in
          let jitem = generate_structure_item local None item in
          loop_without_doctree rest (jitem :: acc)
	| ({str_desc = Tstr_recmodule (mnext :: mrest); _} as item) :: rest -> 
          let item = {item with str_desc = Tstr_recmodule [mnext]} 
          and rest = 
            match mrest with
		[] -> rest
              | _ -> {item with str_desc = Tstr_recmodule mrest} :: rest
          in
          let jitem = generate_structure_item local None item in
          loop_without_doctree rest (jitem :: acc)
	|({str_desc = Tstr_class (cnext :: crest); _} as item) :: rest -> 
          let item = {item with str_desc = Tstr_class [cnext]} 
          and rest =
            match crest with
		[] -> rest
              | _ -> {item with str_desc = Tstr_class crest} :: rest
          in
          let jitem = generate_structure_item local None item in
          loop_without_doctree rest (jitem :: acc)
	| ({str_desc = Tstr_class_type (cnext :: crest); _} as item) :: rest -> 
          let item = {item with str_desc = Tstr_class_type [cnext]}
          and rest = 
            match crest with
		[] -> rest
              | _ -> {item with str_desc = Tstr_class_type crest} :: rest
          in
          let jitem = generate_structure_item local None item in
          loop_without_doctree rest (jitem :: acc) 
	| item :: rest ->
          let jitem = generate_structure_item local None item in
          loop_without_doctree rest (jitem :: acc)
    in
    match dstr_items with 
      | Some ditems -> loop_with_doctree ditems tstr_items [] false
      | None -> loop_without_doctree tstr_items []


  and generate_type_item local name type_decl body info = 
    let open Html_utils in
	let params_variances = html_of_type_param_list 
	  (List.map generate_typ_param type_decl.typ_params) 
	  (List.map generate_variance type_decl.typ_variance) in
  
  (* TODO : make something out of the constraints *)
  (* let cstrs =
     List.map (fun (ct1, ct2, _) -> (generate_typ local ct1, generate_typ local ct2))
     type_decl.typ_cstrs in
  *)
  let priv = 
    match type_decl.typ_private with
        Private -> <:html<$keyword "private"$>>
      | Public -> Cow.Html.nil
  in
  let manifest = 
    match type_decl.typ_manifest, type_decl.typ_kind with
      | Some typ, Ttype_record _ -> 
        let cd = Html.code ~cls:"type" (generate_typ local typ) in
        <:html<= {$cd$}>>
      | Some typ, _ -> 
        let cd = Html.code ~cls:"type" (generate_typ local typ) in
        <:html<= $cd$>>
      | None, Ttype_record _ ->  <:html<= {>>
      | None, Ttype_abstract -> Cow.Html.nil
      | None, _ -> <:html<= >>
  in
					  
  let h_f = 
    match type_decl.typ_manifest, type_decl.typ_kind with
      | None, Ttype_variant _ -> fun x -> Html.pretrack 16 (Html.code x)
      | None, Ttype_record _ -> fun x -> Html.pretrack 17 (Html.code x)
      | _ -> Html.pretrack 18 (* ?? fun x -> <:html<<div>$x$</div>&>> *)
  in
  
  let signature = generate_mark Opam_doc_config.Type name 
    <:html<$keyword "type"$ $params_variances$$str:name$>> in
  let signature = h_f <:html<$signature$ $manifest$$priv$>> in
  
  <:html<$signature$$body$$info$>>

  and generate_value_item name typ info =
    let open Html_utils in
      let signature = generate_mark Opam_doc_config.Value 
	name <:html<$keyword "val"$ $str:name$>> in
      let cd = Html.code ~cls:"type" typ in
      let signature = Html.pretrack 19 <:html<$signature$ : $cd$>> in
      <:html<$signature$$info$>>

  and generate_exception_item name args info =
      let open Html_utils in
	  let id = generate_mark Opam_doc_config.Exception name
	    <:html<$keyword "exception"$ $str:name$>> in 
	  let args = match args with 
	    | [] -> Cow.Html.nil 
	    | _ -> 
              let cd = Html.code ~cls:"type" (Html.concat ~sep:" * " args) in
              <:html< $keyword "of"$ $cd$>> in
	  let signature = Html.pretrack 20 <:html<$id$$args$>> in
	  <:html<$signature$$info$>>

  and generate_module_item name module_result item_info =
      let open Html_utils in
	  let reference = <:html<<a href="$uri:current_module_uri ()$">$str:name$</a>&>> in

	  (* TODO constraints *)

	  match module_result with 
	    | Ident (body, Gentyp.Resolved (uri, _)) -> 
              let cd = Html.code ~cls:"type" body in
	      let signature = Html.pretrack 21 <:html<$keyword "module"$ $reference$ : $cd$>> in 
	      <:html<<div class="ocaml_module ident" name="$str:name$" path="$uri:uri$">$signature$$item_info$</div>&>>			
	    | Ident (body, Gentyp.Unresolved _) | Ident (body, _) -> 
              let cd = Html.code ~cls:"type" body in
	      let signature = Html.pretrack 22 <:html<$keyword "module"$ $reference$ : $cd$>> in 
	      <:html<<div class="ocaml_module ident" name="$str:name$">$signature$$item_info$</div>&>>
	    | Sig (body, content) ->
              let cd = Html.code ~cls:"type" body in
	      let signature = Html.pretrack 23 <:html<$keyword "module"$ $reference$ : $cd$>> in 
	      <:html<<div class="ocaml_module sig" name="$str:name$">$signature$$item_info$$content$</div>&>>

  and generate_module_type_item name module_result item_info =
      let open Html_utils in
	  let reference = <:html<<a href="$uri:current_module_uri ()$">$str:name$</a>&>> in

	  match module_result with 
	    | Ident (body, Gentyp.Resolved (uri, _)) -> 
              let cd = Html.code ~cls:"type" body in
	      let signature = Html.pretrack 24 <:html<$keyword "module type"$ $reference$ = $cd$>> in 
	      <:html<<div class="ocaml_module ident" name="$str:name$" path="$uri:uri$">$signature$$item_info$</div>&>>			
	    | Ident (body, Gentyp.Unresolved _) | Ident (body, _) -> 
              let cd = Html.code ~cls:"type" body in
	      let signature = Html.pretrack 25 <:html<$keyword "module type"$ $reference$ = $cd$>> in 
	      <:html<<div class="ocaml_module ident" name="$str:name$">$signature$$item_info$</div>&>>
	    | Sig (body, content) ->
              let cd = Html.code ~cls:"type" body in
	      let signature = Html.pretrack 26 <:html<$keyword "module type"$ $reference$ = $cd$>> in 
	      <:html<<div class="ocaml_module sig" name="$str:name$">$signature$$item_info$$content$</div>&>>
	
  and generate_include_item module_result typ_sig item_info =
      let open Html_utils in
	  let included_items = js_array_of_include_items typ_sig in
	  
	  match module_result with 
	    | Ident (body, Gentyp.Resolved (uri, _)) -> 
	      let signature = 
                let cd = Html.code ~cls:"type" body in
		Html.pretrack 27 <:html<$keyword "include"$ $cd$>> in
	      <:html<<div class="ocaml_include ident" path=$uri:uri$ items="$str:included_items$">$signature$$item_info$</div>&>>
			    
	    | Ident (body, Gentyp.Unresolved _) | Ident (body, _) -> 
	      let signature = 
                let cd = Html.code ~cls:"type" body in
		Html.pretrack 28 <:html<$keyword "include"$ $cd$>> in
	      <:html<<div class="ocaml_include ident" items="$str:included_items$">$signature$$item_info$</div>&>>	      
	    | Sig (body, content) ->
	      let signature = 
                let cd = Html.code ~cls:"type" body in
		Html.pretrack 29 <:html<$keyword "include"$ $cd$>> in
	      <:html<<div class="ocaml_include sig" items="$str:included_items$">$signature$$item_info$$content$</div>&>>
			    
 and generate_class_item name params variance virt class_result item_info =
    let open Html_utils in
	let id = generate_mark Opam_doc_config.Type name in
	let params_html = html_of_type_class_param_list params variance in
	
	let reference = 
	    <:html<<a href="$uri:current_class_uri name$">$str:name$</a>&>> in

	let signature = <:html<$keyword "class"$>> in
	let signature = 
	  <:html<$signature$ $if virt then keyword "virtual " else Cow.Html.nil$>> in
	let signature = <:html<$signature$$params_html$$reference$>> in
	
	match class_result with 
	  | Ident (s, p) -> 
	    let signature = Html.pretrack 30 <:html<$id signature$ : $s$>> in
	    begin 
	      match p with 
		| Gentyp.Resolved (uri, _) -> <:html<<div class="ocaml_class ident" name="$str:name$" path="$uri:uri$">$signature$$item_info$</div>&>>
                | Gentyp.Unresolved _ | _ -> <:html<<div class="ocaml_class ident" name="$str:name$">$signature$</div>&>>	   
             end 
          | Sig (s, content) ->
	    let signature = Html.pretrack 31 <:html<$id signature$ : $s$>> in
	    <:html<<div class="ocaml_class sig" name="$str:name$">$signature$$item_info$$content$</div>&>>
			 

 and generate_class_type_item name params variance virt class_result item_info =
    let open Html_utils in
	let id = generate_mark Opam_doc_config.Type name in
	let params_html = html_of_type_class_param_list params variance in
		
	let reference = 
	    <:html<<a href="$uri:current_class_uri name$">$str:name$</a>&>> in

	let signature = <:html<$keyword "class type"$ >> in
	let signature = 
	  <:html<$signature$$if virt then keyword "virtual " else Cow.Html.nil$>> in
	let signature = <:html<$signature$$params_html$$reference$>> in
	
	match class_result with 
	  | Ident (s, p) -> 
	    let signature = Html.pretrack 32 <:html<$id signature$ : $s$>> in
	    begin 
	      match p with 
		| Gentyp.Resolved (uri, _) -> <:html<<div class="ocaml_class ident" name="$str:name$" path="$uri:uri$">$signature$$item_info$</div>&>>
                | Gentyp.Unresolved _ | _ -> <:html<<div class="ocaml_class ident" name="$str:name$">$signature$$item_info$</div>&>>	   
             end 
          | Sig (s, content) ->
	    let signature = Html.pretrack 33 <:html<$id signature$ : $s$>> in
	    <:html<<div class="ocaml_class sig" name="$str:name$">$signature$$item_info$$content$</div>&>>
			 
			 
  and generate_signature_item local (ditem : Doctree.signature_item option) item : Cow.Html.t= 
    let open Html_utils in
    let ditem_desc, item_info = match ditem with 
      | Some {dsig_desc=desc; dsig_info=i1; dsig_after_info=i2} ->
	Some desc, 
	make_info (generate_info_opt2 local i1 i2)
      | None -> None, Cow.Html.nil
    in
    
    match ditem_desc, item.sig_desc with
      | (None | Some (Dsig_value _)), Tsig_value(id, _, val_desc) -> 
        (* Special treatment for primitives ? *)
	(* begin match val_desc.val_prim with
          | [] -> 
            let jtyp = generate_typ local val_desc.val_desc in
            iValue name jtyp jinfo
          | primitive ->
            let jtyp = generate_typ local val_desc.val_desc in
            
	   iPrimitive name jtyp primitive jinfo end *)
	let typ = generate_typ local val_desc.val_desc in

	generate_value_item id.Ident.name typ item_info
	  
      | dsig_type, Tsig_type [id, _, tdecl] ->
	add_internal_reference id;
	
	let body = generate_type_kind local id.Ident.name
	  (match dsig_type with | Some (Dsig_type (_, dkind)) -> Some dkind | _ -> None)
	  tdecl.typ_kind in

	generate_type_item local id.Ident.name tdecl body item_info
	    
    | (None | Some (Dsig_exception _)), Tsig_exception(id, _, edecl) ->

      let args = List.map (generate_typ local) edecl.exn_params in
      generate_exception_item id.Ident.name args item_info

    | dsig_module, (Tsig_module(id, _, mty) | Tsig_recmodule ((id, _, mty)::_)) -> 
      add_internal_reference id;
      
      let prev_env = !glob_env in 
      glob_env := !glob_env @ [id.Ident.name];

      let module_result = generate_module_type local 
	(match dsig_module with
	  | Some (Dsig_module(_, dmty)) | Some (Dsig_recmodule (_, dmty)) -> Some dmty
	  | _ -> None)
	mty in

      let res = generate_module_item id.Ident.name module_result item_info in
      glob_env := prev_env;
      res

    | dsig_modtype, Tsig_modtype(id, _, tmtydecl) -> 
      add_internal_reference id;
      
      let prev_env = !glob_env in 
      glob_env := !glob_env @ [id.Ident.name];
      
      let module_result = 
	match tmtydecl with 
	  | Tmodtype_manifest mty -> 
	    generate_module_type local 
	      (match dsig_modtype with
		| Some (Dsig_modtype (_,dmty)) -> dmty | None | _ -> None)
	      mty
	  | Tmodtype_abstract -> Sig (Cow.Html.nil, Cow.Html.nil) (* to check *)
      in
      
      let res = generate_module_type_item id.Ident.name module_result item_info in
      glob_env := prev_env;
      res

    | dsig_include, Tsig_include(mty, typ_sig) ->
      
      add_include_references typ_sig;
      
      let module_result = generate_module_type local 
	(match dsig_include with | Some (Dsig_include dmty) -> Some dmty | None | _ -> None)
	mty in
      
      generate_include_item module_result mty.mty_type item_info

    | dsig_class, Tsig_class [cl_desc] -> 
      (match cl_desc with
	| { ci_id_class=id1; ci_id_class_type=id2;
	    ci_id_object=id3; ci_id_typesharp=id4; _ } ->
	  List.iter add_internal_reference [id1; id2; id3; id4]);
      
      let params = List.map generate_class_param (fst cl_desc.ci_params) in
      let variance = List.map generate_variance cl_desc.ci_variance in
      let virt = 
        match cl_desc.ci_virt with
            Virtual -> true
          | Concrete -> false
      in
      let class_result = generate_class_type local
	(match dsig_class with Some (Dsig_class (_, dclty)) -> Some dclty | None | _ -> None)
	cl_desc.ci_expr in
      
      let name = cl_desc.ci_id_class.Ident.name in

      generate_class_item name params variance virt class_result item_info

    | dsig_class_type, Tsig_class_type [clty_decl] ->
      (match clty_decl with
	| { ci_id_class=id1; ci_id_class_type=id2;
	    ci_id_object=id3; ci_id_typesharp=id4; _ } ->
	  List.iter add_internal_reference [id1; id2; id3; id4]);    
      
      let params = List.map generate_class_param (fst clty_decl.ci_params) in
      let variance = List.map generate_variance clty_decl.ci_variance in
      let virt = match clty_decl.ci_virt with | Virtual -> true | Concrete -> false in
      let class_result = generate_class_type local 
	(match dsig_class_type with
	  | Some (Dsig_class_type (_, dclty)) -> Some dclty | None | _ -> None)
	clty_decl.ci_expr in
      
      generate_class_type_item clty_decl.ci_id_class.Ident.name 
	params variance virt class_result item_info

    | _ , _ -> 
      Printf.eprintf "[Warning] Mismatching items\n%!";
      Cow.Html.nil

  and generate_structure_item local (ditem : Doctree.structure_item option) item : Cow.Html.t = 
      let open Html_utils in
      let ditem_desc, item_info = match ditem with 
      | Some {dstr_desc=desc; dstr_info=i1} ->
	Some desc, 
	make_info (generate_info_opt local i1)
      | None -> None, Cow.Html.nil
      in
      
      match ditem_desc, item.str_desc with
	| (None | Some (Dstr_value _)) , Tstr_value (rec_flag, [(patt,_)])  ->
	  let rec get_pattern_name = function
	    | {pat_desc=Tpat_var (id, _); _} -> id.Ident.name
	    | {pat_desc=Tpat_tuple patl; _}-> 
	      "(" ^ String.concat ", " (List.map get_pattern_name patl) ^")"
	    | {pat_desc=Tpat_construct _; _} -> "()"
	    | _ -> "Unknown name"
	  in
	  begin
	  match patt.pat_desc with
	    | Tpat_construct _ -> Cow.Html.nil (* skipping the 'let () = ...' elements *)
	    | _ -> 
	      let name = get_pattern_name patt in
	      let typ = Gentyp.type_scheme local patt.pat_type in
	      generate_value_item name typ item_info
	  end	    

	| _, Tstr_value _ -> 
	  Printf.eprintf "Unsupported value representation\n%!";
	  Cow.Html.nil

	| dstr_type, Tstr_type [id, _, tdecl] -> 
	  
	  add_internal_reference id;

	  let body = generate_type_kind local id.Ident.name
	    (match dstr_type with | Some (Dstr_type (_, dkind)) -> Some dkind | _ -> None)
	    tdecl.typ_kind in
	  
	  generate_type_item local id.Ident.name tdecl body item_info

	| (None | Some (Dstr_exception _)), Tstr_exception(id, _, edecl) ->
	  let args = List.map (generate_typ local) edecl.exn_params in
	  
	  generate_exception_item id.Ident.name args item_info

	| dstr_module, (Tstr_module(id, _, mtexpr) 
			   | Tstr_recmodule ((id, _, _, mtexpr)::_)) -> 

	  add_internal_reference id;
	  
	  let prev_env = !glob_env in 
	  glob_env := !glob_env @ [id.Ident.name];      
	  	  
	  let module_result = generate_module_expr local 
	    (match dstr_module with
	      | Some (Dstr_module(_, dmexpr)) 
	      | Some (Dstr_recmodule (_, _, dmexpr)) -> Some dmexpr
	      | _ -> None)
	    mtexpr in

	  let res = generate_module_item id.Ident.name module_result item_info in
	  glob_env := prev_env;
	  res
	    
	| dstr_modtype, Tstr_modtype(id, _, tmty) ->

	  add_internal_reference id;
	  
	  let prev_env = !glob_env in 
	  glob_env := !glob_env @ [id.Ident.name];      
	  	  
	  let module_result = 
	    generate_module_type local 
	      (match dstr_modtype with
		| Some (Dstr_modtype (_,dmty)) -> Some dmty | None | _ -> None)
	      tmty
	  in
	  
	  let res =  generate_module_type_item id.Ident.name module_result item_info in
	  glob_env := prev_env;
	  res
	
	| dstr_include, Tstr_include(mexpr, typ_sig) ->
	  
	  add_include_references typ_sig;
	  
	  let module_result = generate_module_expr local 
	    (match dstr_include with 
	      | Some (Dstr_include dmexpr) -> Some dmexpr | None | _ -> None)
	    mexpr in
	  
	  generate_include_item module_result mexpr.mod_type item_info

	| dstr_class, Tstr_class [cl_desc, _, _] -> 	    

	  (match cl_desc with
	    | { ci_id_class=id1; ci_id_class_type=id2;
		ci_id_object=id3; ci_id_typesharp=id4; _ } ->
	      List.iter add_internal_reference [id1; id2; id3; id4]);

	  let params = List.map generate_class_param (fst cl_desc.ci_params) in
	  let variance = List.map generate_variance cl_desc.ci_variance in
	  let virt = 
            match cl_desc.ci_virt with
		Virtual -> true
              | Concrete -> false
	  in
	  let class_result = generate_class_expr local
	    (match dstr_class with 
	      | Some (Dstr_class (_, dclexpr)) -> Some dclexpr | None | _ -> None)
	    cl_desc.ci_expr in
	  
	  let name = cl_desc.ci_id_class.Ident.name in

	  generate_class_item name params variance virt class_result item_info

	| dstr_class_type, Tstr_class_type [(id, _, clty_decl)] ->
	  add_internal_reference id;
	  
	  let params = List.map generate_class_param (fst clty_decl.ci_params) in
	  let variance = List.map generate_variance clty_decl.ci_variance in
	  let virt = match clty_decl.ci_virt with | Virtual -> true | Concrete -> false in
	  let class_result = generate_class_type local 
	    (match dstr_class_type with
	      | Some (Dstr_class_type (_, dclty)) -> Some dclty | None | _ -> None)
	    clty_decl.ci_expr in
	  
	  generate_class_type_item id.Ident.name params variance virt class_result item_info

	| (None | Some (Dstr_primitive _)) , Tstr_primitive (id, _, val_desc) -> 
	  let typ = generate_typ local val_desc.val_desc in
	  generate_value_item id.Ident.name typ item_info

	| (None | Some (Dstr_exn_rebind _)), Tstr_exn_rebind (id, _, path, _) ->
	  Printf.eprintf "Exception rebind : Not yet implemented\n%!";
	  Cow.Html.nil
	    
	| (None | Some Dstr_eval), Tstr_eval _ -> Cow.Html.nil
	| _, _ -> 
	  Printf.eprintf "[Warning] Mismatching items\n%!"; Cow.Html.nil


let output_toplevel_module module_name html_elements =
  let filename = Opam_doc_config.current_package () ^ "/" ^  module_name ^ ".html" in
  let oc = open_out filename in
  output_string oc "<div class=\"ocaml_toplevel_module\">";
  List.iter 
    (fun e -> output_string oc ((Html.string_of_html e) ^ "\n"))
    html_elements;
  output_string oc "</div>";
  close_out oc

let generate_file_from_interface local module_name doctree intf =
  glob_env := [module_name];
  let ditems_opt, info = match doctree with 
      | Some (Dfile_intf dintf) -> 
	Some dintf.dintf_items, 
	generate_info_opt local dintf.dintf_info
      | None | _ -> None, None in
  
  let items = generate_signature_item_list local ditems_opt intf.sig_items in
  let items,descr = match info with None -> items, Cow.Html.nil | Some i -> i :: items, i in

  output_toplevel_module module_name items;
  module_name, descr

let generate_file_from_structure local module_name doctree impl =
  glob_env := [module_name];
  let ditems_opt, info = match doctree with 
    | Some (Dfile_impl dimpl) -> 
      Some dimpl.dimpl_items,
      generate_info_opt local dimpl.dimpl_info
    | None | _ -> None, Some Cow.Html.nil in
  
  let items = generate_structure_item_list local ditems_opt impl.str_items in
  let items,descr = match info with None -> items, Cow.Html.nil | Some i -> i :: items, i in
  
  output_toplevel_module module_name items;
  module_name, descr
(* (Html_utils.make_first_line_info info) *)
  

