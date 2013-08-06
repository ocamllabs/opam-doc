open Docjson
open Info
open Doctree
open Typedtree
open Location
open Asttypes
open Cow

open Types
    
let get_path local ?(is_class=false) (p : Path.t) =
  Gentyp_html.path local is_class p
    
let path_to_html = Gentyp_html.html_of_path

let generate_html_path local ?(is_class=false) (p : Path.t) : Html.t =
  let path = get_path local ~is_class:is_class p in
  Gentyp_html.html_of_path path


type doc_env = 
    {current_module_name:string;
     parent_modules:string list (** reversed list B.M.SubM -> ["M"; "B"] *) }

(* doc_env's abstraction layer *)
let new_env module_name = 
  {current_module_name=module_name; parent_modules=[]}

let add_to_env env module_name =
  {current_module_name=module_name; 
   parent_modules=env.current_module_name::env.parent_modules}

let get_full_path_name env =
  String.concat "." (List.rev (env.current_module_name::env.parent_modules))

(* TO REMOVE:
let internal_path = ref []

let generate_submodule name f arg =
  internal_path:=name::(!internal_path);
  let res = f arg in
  internal_path:=List.tl !internal_path;  
  res

*)
(*
let add_internal_reference id =
   Index.add_internal_reference id (List.rev !internal_path)

let rec treat_module_type id = function
  | Mty_ident p -> () (* should do something ? *)
  | Mty_signature msig -> generate_submodule 
    id.Ident.name add_include_references msig
  | Mty_functor (_,_,mtyp) -> treat_module_type id mtyp (* should do more ?*)


and add_include_references sig_list = 
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
  *)      
 
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
    Html.nil
    text

and generate_list_items local items =
  List.fold_left 
    (fun acc item -> <:html<$acc$<li>$generate_text local item$</li>&>>)
    Html.nil
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
    | _ -> Html.nil in
  let id = match lbl with 
    | Some s ->  s
    | _ -> sn^"_"^Opam_doc_config.mark_title
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
      Html.nil
      sees in
  <:html<<b>See also</b> <ul>$elems$</ul>&>>

let generate_befores local befores = 
  let gen_before (s, t) =
    <:html<<b>Before $str:s$</b> $generate_text local t$>>
  in
    List.fold_left 
      (fun acc before -> <:html<$acc$$gen_before before$<br/>&>>)
      Html.nil
      befores

let generate_params local params = 
  let gen_param (s, t) =
    <:html<<div class="param_info"><code class="code">$str:s$</code> : $generate_text local t$</div>&>>
  in
    List.fold_left 
      (fun acc param -> <:html<$acc$$gen_param param$&>>)
      Html.nil
      params

let generate_raised local raised = 
  let gen_raised (s, t) =
    <:html<<code>$str:s$</code> $generate_text local t$<br/>&>>
  in
    List.fold_left 
      (fun acc raised -> <:html<$acc$$gen_raised raised$>>)
      Html.nil
      raised


(* TODO add support for custom tags *)
let generate_info local info = 
  let jinfo = 
    match info.i_desc with
	Some t -> generate_text local t
      | None -> Html.nil
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
  Gentyp_html.type_scheme local typ.ctyp_type

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
    | Ttype_abstract -> Html.nil
      
    | Ttype_variant cstrs ->
      let rec loop cstrs dcstrs acc =
	match cstrs with
          | ((_, {txt = name; _}, _, _) as cstr) :: rest ->
            let dcstrl, drest = 
              List.partition (fun (n, _) -> n = name) infos 
            in
            let item =
              match dcstrl with
                | dcstr :: _ -> 
		  generate_variant_constructor local parent_name (snd dcstr) cstr 
		| [] -> generate_variant_constructor local parent_name None cstr
            in
            loop rest drest (item :: acc)
          | [] -> 
            if dcstrs <> [] then raise (Failure "generate_type_kind : Unknown Constructor")
            else List.rev acc
      in
      let items = loop cstrs infos [] in
      Html_utils.make_type_table (fun x -> x) items
	
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
            if dlbls <> [] then raise (Failure "Unknown Label")
            else List.rev acc
       in
       let items = loop lbls infos [] in
       Html_utils.make_type_table (fun x -> x) items

(** Returns a signature and a path option in order to wrap the content *)
let rec generate_class_type local dclty clty =
  let rec loop local dclty clty args_acc =
    match dclty, clty.cltyp_desc with
      | (Some Dcty_constr|None), Tcty_constr(path, _, cor_list) ->
	let params = List.map (generate_typ local) cor_list in
	
	let path = get_path local ~is_class:true path in
	let html_path = path_to_html path in
	
	let args = 
	  Html_utils.code "type" (List.fold_left 
				    (fun acc typ -> <:html<$acc$$typ$ -> >>) 
				    Html.nil (List.rev args_acc)) in
	
	let params = 
	  Html_utils.html_of_type_class_param_list
	    params (List.map (fun _ -> `None) params) (* dummy variance list *)
	in

	let body = <:html<$args$$params$$html_path$>> in
	
	body, Some path
	  
      | dclass_sig, Tcty_signature class_sig ->
	let fields : Html.t list = 
	  generate_class_type_fields local 
	    (match dclass_sig with Some Dcty_signature cl -> Some cl | _ -> None)
	    (* The fields are reversed... Why? 
	       Still true? => TEST *)
	    (List.rev class_sig.csig_fields) in
	
	let args = match args_acc with
	  | [] -> Html.nil 
	  | l -> Html_utils.code "type"
	    (List.fold_left (fun acc typ -> <:html<$acc$$typ$ -> >>) Html.nil l)
	in 
	
	let body = let open Html_utils in 
	     <:html<$args$$code "code" (html_of_string "object")$ .. $code "code" (html_of_string "end")$>> in
			
	<:html<$body$$Html_utils.create_class_signature_content fields$>>, None
	  
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
    
    let process_class_field local tfield = 
      let open Html_utils in
      match tfield.ctf_desc with
	| Tctf_inher ctyp -> 
	  let ctyp, path = generate_class_type local None ctyp in
	  let signature = make_pre 
	    <:html<$keyword "inherit"$ $ctyp$>> in
	  create_class_container "_inherit_field" signature ctyp path
	| Tctf_val (name, mut_f, virt_f, co_typ) ->  
	  let typ = generate_typ local co_typ in
	  let mut = match mut_f with | Mutable -> true | Immutable -> false in
	  let virt = match virt_f with | Virtual -> true | Concrete -> false in
	  let label = keyword "val" in
	  let label = 
	    if virt then <:html<$label$ $keyword "virtual"$>> else label in
	  let label = 
	    if mut then <:html<$label$ $keyword "mutable"$>> else label in
	  let label = generate_mark Opam_doc_config.mark_attribute name 
	    <:html<$label$ $str:name$>> in
	  make_pre <:html<$label$ : $code "code" typ$>>		   
	| Tctf_meth (name, priv_f, co_typ) -> 
	  let typ = generate_typ local co_typ in
	  let priv = match priv_f with Private -> true | Public -> false in
	  
	  let label = keyword "method" in
	  let label = 
	    if priv then <:html<$label$ $keyword "private"$>> else label in
	  let label = generate_mark Opam_doc_config.mark_method 
	    name <:html<$label$ $str:name$>> in
	  make_pre <:html<$label$ : $code "code" typ$>>
	| Tctf_virt (name, priv_f, co_typ) -> 
	  let typ = generate_typ local co_typ in
	  let priv = match priv_f with Private -> true | Public -> false in
	  
	  let label = keyword "method" in
	  let label = <:html<$label$ $keyword "virtual"$>> in
	  let label = 
	    if priv then <:html<$label$ $keyword "private"$>> else label in
	  let label = generate_mark Opam_doc_config.mark_method 
	    name <:html<$label$ $str:name$>> in
	  make_pre <:html<$label$ : $code "code" typ$>>
	| Tctf_cstr (co_typ1, co_typ2) ->
	  let jtyp1 = generate_typ local co_typ1 in
	  let jtyp2 = generate_typ local co_typ2 in
	  let label = <:html<$jtyp1$ = $jtyp2$>> in
	  let label = <:html<$code "type" label$>> in
	  make_pre <:html<$keyword "constraint"$ $label$>>	  
    in
    
    let generate_class_type_fields_with_doctree local dclsigl tclsigl =
      let rec loop (acc: Html.t list) local dclsigl tclsigl is_stopped =
    	match dclsigl, tclsigl with
	  | [], r -> 
	    Printf.eprintf "generate_class_type_fields mismatch -- processing without doc";
	    List.rev acc @ List.map (process_class_field local) r
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
		    let open Html_utils in
		    let ctyp, path = generate_class_type local None ctyp in
		    let signature = make_pre 
		      <:html<$keyword "inherit"$ $ctyp$>> in
		    create_class_container "_inherit_field" signature ctyp path
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
		  Printf.eprintf "generate_class_type_fields mismatch -- processing without doc";
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
	  | [] -> Html.nil 
	  | l -> Html_utils.code "type"
	    (List.fold_left (fun acc typ -> <:html<$acc$$typ$ -> >>) Html.nil l)
	in
	let body = 
	  let open Html_utils in 
	      <:html<$args$$code "code" (html_of_string "object")$ .. $code "code" (html_of_string "end")$>> in
		   
	  <:html<$body$$Html_utils.create_class_signature_content fields$>>, None
	  
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
	  Html_utils.code "type" (List.fold_left 
				    (fun acc typ -> <:html<$acc$$typ$ -> >>) 
				    Html.nil (List.rev args_acc)) in
	let params = 
	  Html_utils.html_of_type_class_param_list
	    params (List.map (fun _ -> `None) params) (* dummy variance list *)
	in

	let body = <:html<$args$$params$$html_path$>> in
	body, Some path

      | dclass_constraint, Tcl_constraint (class_expr, Some ctyp, _, _, _) ->

	let cte, path = loop local 
	  (match dclass_constraint with Some (Dcl_constraint (e,_)) -> Some e | _ -> None)
	  class_expr [] in
	let ctyp, _ = generate_class_type local
	  (match dclass_constraint with Some (Dcl_constraint (_,t)) -> Some t | _ -> None)
	  ctyp in

	let args = match args_acc with
	  | [] -> Html.nil 
	  | l -> Html_utils.code "type"
	    (List.fold_left (fun acc typ -> <:html<$acc$$typ$ -> >>) Html.nil l)
	in 

	<:html<$args$( $cte$ : $ctyp$ )>>, path
	  
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
	      let ctyp, path = generate_class_expr local None cexpr in
	      let signature = make_pre 
		<:html<$keyword "inherit"$ $ctyp$>> in
	      create_class_container "_inherit_field" signature ctyp path
	    | Tcf_val (name, _, mut_f, _, cl_f_kind, _) ->
	      
	      let typ,virt = extract_type_and_virtual_from_kind cl_f_kind in
	      let mut = match mut_f with | Mutable -> true | Immutable -> false in
	      
	      let label = keyword "val" in
	      let label = 
		if virt then <:html<$label$ $keyword "virtual"$>> else label in
	      let label = 
		if mut then <:html<$label$ $keyword "mutable"$>> else label in
	      let label = generate_mark Opam_doc_config.mark_attribute name 
		<:html<$label$ $str:name$>> in
	      make_pre <:html<$label$ : $code "code" typ$>>		   
	    
	    | Tcf_meth (name, _, priv_f, cl_f_kind, ovr_b) ->
	      let typ, virt = extract_type_and_virtual_from_kind cl_f_kind in
	      let priv = match priv_f with Private -> true | Public -> false in
	      	      
	      let label = keyword "method" in
	      let label = 
		if virt then <:html<$label$ $keyword "virtual"$>> else label in
	      let label = 
		if priv then <:html<$label$ $keyword "private"$>> else label in
	      let label = generate_mark Opam_doc_config.mark_method 
		name <:html<$label$ $str:name$>> in
	      make_pre <:html<$label$ : $code "code" typ$>>
	    | Tcf_constr (co_typ1, co_typ2) ->
	      let jtyp1 = generate_typ local co_typ1 in
	      let jtyp2 = generate_typ local co_typ2 in
	      let label = <:html<$jtyp1$ = $jtyp2$>> in
	      let label = <:html<$code "type" label$>> in
	      make_pre <:html<$keyword "constraint"$ $label$>>	  
	    | Tcf_init _ -> Html.nil
      in
      
      let generate_class_type_fields_with_doctree local dclexpr tclexpr =
	let rec loop (acc: Html.t list) local dclsigl tclsigl is_stopped =
    	  match dclsigl, tclsigl with
	    | [], r -> 
	      Printf.eprintf "generate_class_type_fields mismatch -- processing without doc";
	      List.rev acc @ List.map (process_class_field local) r
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
			  let ctyp, path = generate_class_expr local None cexpr in
			  let signature = make_pre 
			    <:html<$keyword "inherit"$ $ctyp$>> in
			  create_class_container "_inherit_field" signature ctyp path
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
		    Printf.eprintf "generate_class_type_fields mismatch -- processing without doc";
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
      <:html<$code "code" path$&>>, Some real_path

    | Some (Dmty_signature dsg), Tmty_signature sg ->
      let sg_items = generate_signature_item_list local (Some dsg) sg.sig_items in
      let signature = 
	<:html<$code "code" (html_of_string "sig")$ .. $code "code" (html_of_string "end")$>>
      in
      let module_content = create_module_signature_content sg_items in
      <:html<$signature$$module_content$>>, None
    | None, Tmty_signature sg ->
      let sg_items = generate_signature_item_list local None sg.sig_items in
      let signature = 
	<:html<$code "code" (html_of_string "sig")$ .. $code "code" (html_of_string "end")$>>
      in
      let module_content = create_module_signature_content sg_items in
      <:html<$signature$$module_content$>>, None
	
    | Some (Dmty_functor(darg, dbase)), Tmty_functor(id, _, arg, base) ->
      let arg, _ = generate_module_type local (Some darg) arg in
      let base, path_opt = generate_module_type local (Some dbase) base in
      
      let label = <:html<$code "code" (html_of_string "functor (")$>> in
      let label = <:html<$label$$code "code" (html_of_string id.Ident.name)$>> in
      let label = <:html<$label$<code class="code"> : </code>&>> in
      let label = <:html<$label$$arg$$code "code" (html_of_string ") -> ")$>> in
      
      let body = <:html<<div class="sig_block">$label$$base$</div>&>> in
      body, path_opt
    | None, Tmty_functor(id, _, arg, base) ->
      let arg, _ = generate_module_type local None arg in
      let base, path_opt = generate_module_type local None base in
      
      let label = <:html<$code "code" (html_of_string "functor (")$>> in
      let label = <:html<$label$$code "code" (html_of_string id.Ident.name)$>> in
      let label = <:html<$label$<code class="code"> : </code>&>> in
      let label = <:html<$label$$arg$$code "code" (html_of_string ") -> ")$>> in
      
      let body = <:html<<div class="sig_block">$label$$base$</div>&>> in
      body, path_opt (* TODO *)
	
    | Some (Dmty_with dbase), Tmty_with(base, cnstrs) ->
      let base, path_opt = generate_module_type local (Some dbase) base in
      let cnstrs = List.map (generate_with_constraint local) cnstrs in

      <:html<$base$ with $insert_between " and " cnstrs$>>, path_opt      
    (* kModTypeWith jcnstrs jbase *)
    | None, Tmty_with(base, cnstrs) ->
      let base, path_opt = generate_module_type local None base in
      let cnstrs = List.map (generate_with_constraint local) cnstrs in
      
      <:html<$base$ with $insert_between " and " cnstrs$>>, path_opt

    | Some (Dmty_typeof dexpr), Tmty_typeof mexpr ->
      let expr, path_opt = generate_module_expr local (Some dexpr) mexpr in
      
      <:html<module type of $expr$>>, path_opt
    | None, Tmty_typeof mexpr ->
      let expr, path_opt = generate_module_expr local None mexpr in

      <:html<module type of $expr$>>, path_opt
      
    | _, _ -> raise (Failure "generate_module_type: Mismatch")
      
and generate_module_expr local dmexpr tmexpr = 
  let open Html_utils in
  match dmexpr, tmexpr.mod_desc with
    | (Some Dmod_ident | None), Tmod_ident(path, _) -> 
      let real_path = get_path local path in
      let path = path_to_html real_path in
      <:html<$code "code" path$&>>, Some real_path

    | Some (Dmod_structure dstr), Tmod_structure str ->
      let str_items = generate_structure_item_list local (Some dstr) str.str_items in
      let signature = 
	<:html<$code "code" (html_of_string "sig")$ .. $code "code" (html_of_string "end")$>>
      in
      let module_content = create_module_signature_content str_items in
      <:html<$signature$$module_content$>>, None
    | None, Tmod_structure str ->
      let str_items = generate_structure_item_list local None str.str_items in
      let signature = 
	<:html<$code "code" (html_of_string "sig")$ .. $code "code" (html_of_string "end")$>>
      in
      let module_content = create_module_signature_content str_items in
      <:html<$signature$$module_content$>>, None
	
    | Some (Dmod_functor(darg, dbase)), Tmod_functor(id, _, arg, base) ->
      let arg, _ = generate_module_type local (Some darg) arg in
      let base, path_opt = generate_module_expr local (Some dbase) base in
      
      let label = <:html<$code "code" (html_of_string "functor (")$>> in
      let label = <:html<$label$$code "code" (html_of_string id.Ident.name)$>> in
      let label = <:html<$label$<code class="code"> : </code>&>> in
      let label = <:html<$label$$arg$$code "code" (html_of_string ") -> ")$>> in
      
      let body = <:html<<div class="sig_block">$label$$base$</div>&>> in
      body, path_opt
    | None, Tmod_functor(id, _, arg, base) ->
      let arg, _ = generate_module_type local None arg in
      let base, path_opt = generate_module_expr local None base in
      
      let label = <:html<$code "code" (html_of_string "functor (")$>> in
      let label = <:html<$label$$code "code" (html_of_string id.Ident.name)$>> in
      let label = <:html<$label$<code class="code"> : </code>&>> in
      let label = <:html<$label$$arg$$code "code" (html_of_string ") -> ")$>> in
      
      let body = <:html<<div class="sig_block">$label$$base$</div>&>> in
      body, path_opt
	
    | Some (Dmod_apply (dmexpr1, dmexpr2)), Tmod_apply (tmexpr1, tmexpr2, _) ->
      let base, path_opt = generate_module_expr local (Some dmexpr1) tmexpr1 in
      let arg, path_opt2 = generate_module_expr local (Some dmexpr2) tmexpr2 in
      <:html<$base$($arg$)>>, path_opt
    | None, Tmod_apply (tmexpr1, tmexpr2, _) -> 
      let base, path_opt = generate_module_expr local None tmexpr1 in
      let arg, path_opt2 = generate_module_expr local None tmexpr2 in
      <:html<$base$($arg$)>>, path_opt

    | dmod_constraint_opt, Tmod_constraint(_, _, Tmodtype_explicit tmty, _) ->
      begin 
	match dmod_constraint_opt with
	  | Some (Dmod_constraint (_, dmty)) -> generate_module_type local (Some dmty) tmty 
	  | None -> generate_module_type local None tmty 
	  | _ -> assert false
      end
    | dmod_constraint_opt, Tmod_constraint(tmexpr, _, Tmodtype_implicit, _) ->
      begin 
	match dmod_constraint_opt with
	  | Some (Dmod_constraint (dmexpr, _)) -> 
	    generate_module_expr local (Some dmexpr) tmexpr
	  | None -> generate_module_expr local None tmexpr 
	  | _ -> assert false
      end
    | (Some Dmod_unpack | None), Tmod_unpack(tmexpr, tmty) -> 
      (* Not sure what to do with an unpack module (first class module) *)
      begin
	match tmty with 
	  | Mty_ident path -> 
	    let real_path = get_path local path in
	    let path = path_to_html real_path in
	    <:html<$code "code" path$&>>, Some real_path
	  | _ -> 	  
	    Printf.eprintf "generate_module_expr: unpack mismatch\n%!";
	    Html.nil, None
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
	| _, _ -> raise (Failure "generate_signature_item_list: Mismatch")
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
      tstr_items : Html.t list =

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
	| ditem :: drest, item :: rest ->
          let jitem = generate_structure_item local (Some ditem) item in
          loop_with_doctree drest rest (jitem :: acc) is_stopped
	| _, _ -> raise (Failure "generate_structure_item_list: Mismatch")
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

  and generate_signature_item local (ditem : Doctree.signature_item option) item : Html.t= 
    let open Html_utils in
    let ditem_desc, item_info = match ditem with 
      | Some {dsig_desc=desc; dsig_info=i1; dsig_after_info=i2} ->
	Some desc, 
	make_info (generate_info_opt2 local i1 i2)
      | None -> None, Html.nil
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
	let signature = generate_mark Opam_doc_config.mark_value 
	  id.Ident.name <:html<$keyword "val"$ $str:id.Ident.name$>> in
	let signature = make_pre <:html<$signature$ : $code "type" typ$>> in
	<:html<$signature$$item_info$>>
	  
      | dsig_type, Tsig_type [id, _, tdecl] ->
	(* ------ TODO -----------
	   add_internal_reference id; *)
	let name = id.Ident.name in
	let params_variances = html_of_type_param_list 
	  (List.map generate_typ_param tdecl.typ_params) 
	  (List.map generate_variance tdecl.typ_variance) in
	
	let body = generate_type_kind local name
	  (match dsig_type with | Some (Dsig_type (_, dkind)) -> Some dkind | _ -> None)
	  tdecl.typ_kind in
	
	(* TODO : make something out of the constraints *)
	(* let cstrs =
	   List.map (fun (ct1, ct2, _) -> (generate_typ local ct1, generate_typ local ct2))
           tdecl.typ_cstrs in
	*)

	let priv = 
          match tdecl.typ_private with
              Private -> <:html<$keyword "private"$>>
            | Public -> Html.nil
	in
	
	let manifest = 
          match tdecl.typ_manifest, tdecl.typ_kind with
            | Some typ, Ttype_record _ -> <:html<= {$code "type" (generate_typ local typ)$}>>
            | Some typ, _ -> <:html<= $code "type" (generate_typ local typ)$>>
	    | None, Ttype_record _ ->  <:html<= {>>
            | None, Ttype_abstract -> Html.nil
            | None, _ -> <:html<= >>
	in

       let h_f = 
	 match tdecl.typ_manifest, tdecl.typ_kind with
           | None, Ttype_variant _ | None, Ttype_record _ -> (fun x -> make_pre (code "" x))
           | _ -> make_pre
       in
       
       let signature = generate_mark Opam_doc_config.mark_type name 
	 <:html<$keyword "type"$ $params_variances$$str:name$>> in
       let signature = h_f <:html<$signature$ $manifest$$priv$>> in
       
       <:html<$signature$$body$$item_info$>>
	    
    | (None | Some (Dsig_exception _)), Tsig_exception(id, _, edecl) ->
      let args = List.map (generate_typ local) edecl.exn_params in
      
      let id = generate_mark Opam_doc_config.mark_exception id.Ident.name
	<:html<$keyword "exception"$ $str:id.Ident.name$>> in 
      let args = match args with 
	| [] -> Html.nil 
	| _ -> <:html< $keyword "of"$ $code "type" (insert_between " * " args)$>> in
      let signature = make_pre <:html<$id$$args$>> in
      <:html<$signature$$item_info$>>

    | dsig_module, (Tsig_module(id, _, mty) | Tsig_recmodule [id, _, mty; _]) -> 
      (* ------ TODO -----------
	   add_internal_reference id; *)
            
      (* To handle : let jmty = generate_submodule name (fun () ->
	generate_module_type local dmty mty) () in
      *)

      let name = id.Ident.name in

      let body, path = generate_module_type local 
	(match dsig_module with
	  | Some (Dsig_module(_, dmty)) | Some (Dsig_recmodule (_, dmty)) -> Some dmty
	  | _ -> None)
	mty in
      
      let reference = (* todo *) <:html<$str:name$>> in
      let signature = 
	make_pre <:html<$keyword "module"$ $reference$ : $code "type" body$>> in
      
    (* todo constraints *)
    
      let signature = <:html<$signature$$item_info$>> in
      
      (* wrapping module *)
      begin
	match path with 
	  | Some (Gentyp_html.Resolved (uri, _)) ->   
	    <:html<<div class="ocaml_module ident" name="$str:name$" path="$uri:uri$">$signature$</div>&>>			
	  | Some (Gentyp_html.Unresolved _) ->
	    <:html<<div class="ocaml_module ident" name="$str:name$">$signature$</div>&>>			
	  | _ -> <:html<<div class="ocaml_module sig" name="$str:name$">$signature$</div>&>>
      end			       
    
    | _ , _ -> assert false

  (*
    
    | Dsig_modtype(name, dmtyo), Tsig_modtype(id, _, mtydecl) ->
      add_internal_reference id;
      
      let jmtyo = 
        match dmtyo, mtydecl with
            Some dmty, Tmodtype_manifest mty -> 
              Some (generate_submodule name (fun () ->
		generate_module_type local dmty mty) ())
          | None, Tmodtype_abstract -> None
          | _, _ -> raise (Failure "generate_signature_item>mod_type: Mismatch")
      in
      let jinfo = generate_info_opt2 local ditem.dsig_info ditem.dsig_after_info in
      iModType name jmtyo jinfo
    | Dsig_open, Tsig_open _ -> 
      (* TODO *)
      iComment None
	(*raise (Failure "Not supported")*)
    | Dsig_include dmty, Tsig_include(mty, typ_sig) ->
      add_include_references typ_sig;
      
      let jmty = generate_module_type local dmty mty in
      let jinfo = generate_info_opt2 local ditem.dsig_info ditem.dsig_after_info in
      let mty_type = mty.mty_type in
      let obj = iInclude jmty jinfo in

      Index.add_include_module_type obj mty_type;
      obj
    | Dsig_class(name, dclty), Tsig_class [cl_desc] ->
      (* jparams = class ->['a, 'b]<- point *)
      (match cl_desc with
	| { ci_id_class=id1; ci_id_class_type=id2;
	    ci_id_object=id3; ci_id_typesharp=id4; _ } ->
	  List.iter add_internal_reference [id1; id2; id3; id4]);
      
      let jparams = List.map generate_class_param (fst cl_desc.ci_params) in
      let jvariance = List.map generate_variance cl_desc.ci_variance in
      let virt = 
        match cl_desc.ci_virt with
            Virtual -> true
          | Concrete -> false
      in
      let jclty = generate_class_type local dclty cl_desc.ci_expr in
      let jinfo = generate_info_opt2 local ditem.dsig_info ditem.dsig_after_info in
      iClass name jparams jvariance virt jclty jinfo
    | Dsig_class_type(name, dclty), Tsig_class_type [clty_decl] ->
      (match clty_decl with
	| { ci_id_class=id1; ci_id_class_type=id2;
	    ci_id_object=id3; ci_id_typesharp=id4; _ } ->
	  List.iter add_internal_reference [id1; id2; id3; id4]);    

      let jparams = List.map generate_class_param (fst clty_decl.ci_params) in
      let jvariance = List.map generate_variance clty_decl.ci_variance in
      let virt = 
        match clty_decl.ci_virt with
            Virtual -> true
          | Concrete -> false
      in
      let jclty = generate_class_type local dclty clty_decl.ci_expr in
      let jinfo = generate_info_opt2 local ditem.dsig_info ditem.dsig_after_info in
      iClassType name jparams jvariance virt jclty jinfo
    | _, _ -> raise (Failure "generate_signature_item: Mismatch") *)

  and generate_structure_item local (ditem : Doctree.structure_item option) item : Html.t = 
    assert false
