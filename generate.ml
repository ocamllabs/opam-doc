open Docjson
open Info
open Doctree
open Typedtree
open Location
open Asttypes
open Cow

open Types

let title_id_tag = "TITLE"

let internal_path = ref []

let generate_submodule name f arg =
  internal_path:=name::(!internal_path);
  let res = f arg in
  internal_path:=List.tl !internal_path;  
  res

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
    | Special_ref _ -> raise (Failure "Not implemented")
    | Target _ -> raise (Failure "Not implemented")

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
    | _ -> sn^"_"^title_id_tag
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
  jinfo
    
let generate_info_opt local info =
  match info with
    None -> None
  | Some info -> Some (generate_info local info)

let generate_info_opt2 local info after_info =
  let info = generate_info_opt local info in
  let after_info = generate_info_opt local after_info in
  match info, after_info with
    | None, None -> None
    | Some i, None -> Some i
    | None, Some i -> Some i
    | Some i, Some i2 -> Some <:html<$i$$i2$>> (* todo fix the inclusion of comments *)

(* TODO do proper type printing *)
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

let generate_variant_constructor local (name, info) (_, _, args, _) =
  constructor 
    name 
    (List.map (generate_typ local) args) 
    (generate_info_opt local info)

let generate_record_label local (name, info) (_, _, mut, typ, _) =
  let mut = 
    match mut with
      Mutable -> true
    | Immutable -> false
  in
    label name mut (generate_typ local typ) (generate_info_opt local info)

let generate_with_constraint local (path, _, cstr) =
  let path = Gentyp.path local path in
  match cstr with
    | Twith_type td -> 
      let typ = 
        match td.typ_manifest with
            Some typ -> typ
          | None -> assert false
        in
      kWithType path false (generate_typ local typ)
    | Twith_typesubst td -> 
      let typ = 
        match td.typ_manifest with
            Some typ -> typ
          | None -> assert false
      in
      kWithType path true (generate_typ local typ)
    | Twith_module(p, _) -> kWithMod path false (Gentyp.path local p)
    | Twith_modsubst(p, _) -> kWithMod path true (Gentyp.path local p)

let generate_variance = function
  | true, false -> `Positive
  | false, true -> `Negative
  | _, _ -> `None

let generate_type_kind local dtk tk =
  match dtk, tk with
    Dtype_abstract, Ttype_abstract -> kAbstract
  | Dtype_variant dcstrs, Ttype_variant cstrs ->
      let rec loop cstrs dcstrs acc =
        match cstrs with
        | ((_, {txt = name}, _, _) as cstr) :: rest ->
            let dcstrl, drest = 
              List.partition (fun (n, _) -> n = name) dcstrs 
            in
            let dcstr =
              match dcstrl with
                dcstr :: _ -> dcstr
              | [] -> (name, None)
            in
            let jcstr = generate_variant_constructor local dcstr cstr in
              loop rest drest (jcstr :: acc)
        | [] -> 
            if dcstrs <> [] then raise (Failure "Unknown Constructor")
            else List.rev acc
      in
      let cstrs = loop cstrs dcstrs [] in
        kVariant cstrs
  | Dtype_record dlbls, Ttype_record lbls ->
      let rec loop lbls dlbls acc =
        match lbls with
        | ((_, {txt = name}, _, _, _) as lbl) :: rest ->
            let dlbll, drest = 
              List.partition (fun (n, _) -> n = name) dlbls 
            in
            let dlbl =
              match dlbll with
                dlbl :: _ -> dlbl
		| [] -> (name, None)
            in
            let jlbl = generate_record_label local dlbl lbl in
              loop rest drest (jlbl :: acc)
        | [] -> 
            if dlbls <> [] then raise (Failure "Unknown Label")
            else List.rev acc
      in
      let labels = loop lbls dlbls [] in
        kRecord labels
  | _, _ -> raise (Failure "generate_type_kind: Mismatch")
    
let rec generate_class_type local dclty clty =
  let rec loop local dclty clty args_acc =
    match dclty, clty.cltyp_desc with
      | Dcty_constr, Tcty_constr(path, _, cor_list) ->
	let params = List.map 
	  (generate_typ local)
	  cor_list in
	let path = Gentyp.path local ~is_class:true path in
	kClassIdent (List.rev args_acc) params path
      | Dcty_signature dclass_sig, Tcty_signature class_sig ->
	(* Temporarly remove the class comments until 
	   the dissociation from cmt / cmd is made *)
	let dclass_sig = 
	  List.filter 
	    (function dfield -> 
	      match dfield.dctf_desc with Dctf_comment -> false | _ -> true)
	    dclass_sig in

	let jfields = List.map2 (generate_class_field local) 
	  dclass_sig
	  (* The fields are reversed... Why? *)
	  (List.rev class_sig.csig_fields) in
	
	kClassSig (List.rev args_acc) jfields
      | Dcty_fun dclass_type, Tcty_fun (label, core_type, sub_class_type) ->
	let arg = generate_typ local core_type in
	loop local dclass_type sub_class_type (arg::args_acc)
      | _,_ -> assert false
  in
  loop local dclty clty []

and generate_class_field local dclsig tclsig =
  (* after info ? *)
  match dclsig.dctf_desc, tclsig.ctf_desc with
    | Dctf_inher dctyp, Tctf_inher ctyp -> 
      let jctyp = generate_class_type local dctyp ctyp in
      let jinfo = generate_info_opt local dclsig.dctf_info in
      fInherit jctyp jinfo
    | Dctf_val name, Tctf_val (_, mut_f, virt_f, co_typ) ->
      let jtyp = generate_typ local co_typ in
      let jinfo = generate_info_opt local dclsig.dctf_info in
      let mut = match mut_f with | Mutable -> true | Immutable -> false in
      let virt = match virt_f with | Virtual -> true | Concrete -> false in
      fVal name mut virt jtyp jinfo
    | Dctf_meth dname, Tctf_meth (_,priv_f, co_typ) -> 
      let jtyp = generate_typ local co_typ in
      let jinfo = generate_info_opt local dclsig.dctf_info in
      let priv = match priv_f with Private -> true | Public -> false in
      fMethod dname false priv jtyp jinfo
    | Dctf_meth dname, Tctf_virt (_,priv_f, co_typ) ->
      let jtyp = generate_typ local co_typ in
      let jinfo = generate_info_opt local dclsig.dctf_info in
      let priv = match priv_f with Private -> true | Public -> false in
      fMethod dname true priv jtyp jinfo
    | Dctf_cstr, Tctf_cstr (co_typ1, co_typ2) -> 
      let jtyp1 = generate_typ local co_typ1 in
      let jtyp2 = generate_typ local co_typ2 in
      let jinfo = generate_info_opt local dclsig.dctf_info in
      fConstraint jtyp1 jtyp2 jinfo
    | _,_ -> 
      print_endline "generate_class_field: Mismatch";
      fVal "Error" false false Html.nil None
(* raise (Failure "generate_class_field: Mismatch") *)

let rec generate_module_type local dmty mty = 
  match dmty, mty.mty_desc with
    Dmty_ident, Tmty_ident(p, _) -> kModTypeIdent (Gentyp.path local p)
  | Dmty_signature dsg, Tmty_signature sg ->
    let jsg = generate_signature_item_list local dsg sg.sig_items in
    kModTypeSig jsg
  | Dmty_functor(darg, dbase), Tmty_functor(_, {txt=name}, arg, base) ->
    let jarg = generate_module_type local darg arg in
    let jbase = generate_module_type local dbase base in
    kModTypeFunctor name jarg jbase
  | Dmty_with dbase, Tmty_with(base, cnstrs) ->
      let jbase = generate_module_type local dbase base in
      let jcnstrs = List.map (generate_with_constraint local) cnstrs in
        kModTypeWith jcnstrs jbase
  | Dmty_typeof dexpr, Tmty_typeof expr ->
    let jexpr = generate_module_expr local dexpr expr in
    kModTypeTypeOf jexpr
  | _, _ -> raise (Failure "generate_module_type: Mismatch")

(* TODO remove assumption that typedtree and doctree perfectly match *)
and generate_signature_item_list local ditems items =
  let rec loop ditems items acc =
      match ditems, items with
        [], _ -> List.rev acc
      | ({dsig_desc = Dsig_comment} as ditem) :: drest, _ ->
          let jinfo = generate_info_opt local ditem.dsig_info in
          let jitem = iComment jinfo in
            loop drest items (jitem :: acc)
      | {dsig_desc = Dsig_stop} :: drest, _ ->
	(* TODO *)
	loop drest items acc
      (* raise (Failure "Not supported") *)
      | {dsig_desc = Dsig_open} :: drest, {sig_desc = Tsig_open _} :: rest ->
          loop drest rest acc
      | ({dsig_desc = Dsig_type _} as ditem) :: drest, 
            ({sig_desc = Tsig_type (tnext :: trest)} as item) :: rest -> 
          let item = {item with sig_desc = Tsig_type [tnext]}
          and rest = 
            match trest with
              [] -> rest
            | _ -> {item with sig_desc = Tsig_type trest} :: rest
          in
          let jitem = generate_signature_item local ditem item in
            loop drest rest (jitem :: acc)
      | ({dsig_desc = Dsig_recmodule _} as ditem) :: drest, 
            ({sig_desc = Tsig_recmodule (mnext :: mrest)} as item) :: rest -> 
          let item = {item with sig_desc = Tsig_recmodule [mnext]} 
          and rest = 
            match mrest with
              [] -> rest
            | _ -> {item with sig_desc = Tsig_recmodule mrest} :: rest
          in
          let jitem = generate_signature_item local ditem item in
            loop drest rest (jitem :: acc)
      | ({dsig_desc = Dsig_class _} as ditem) :: drest, 
          ({sig_desc = Tsig_class (cnext :: crest)} as item) :: rest -> 
        let item = {item with sig_desc = Tsig_class [cnext]} 
        and rest = 
          match crest with
              [] -> rest
            | _ -> {item with sig_desc = Tsig_class crest} :: rest
        in
        let jitem = generate_signature_item local ditem item in
        loop drest rest (jitem :: acc)
      | ({dsig_desc = Dsig_class_type _} as ditem) :: drest, 
        ({sig_desc = Tsig_class_type (cnext :: crest)} as item) :: rest -> 
        let item = {item with sig_desc = Tsig_class_type [cnext]}
        and rest = 
          match crest with
              [] -> rest
            | _ -> {item with sig_desc = Tsig_class_type crest} :: rest
        in
        let jitem = generate_signature_item local ditem item in
        loop drest rest (jitem :: acc)
      | ditem :: drest, item :: rest ->
        let jitem = generate_signature_item local ditem item in
        loop drest rest (jitem :: acc)
      | _, _ -> raise (Failure "generate_signature_item_list: Mismatch")
  in
  loop ditems items []

and generate_signature_item local ditem item =
  match ditem.dsig_desc, item.sig_desc with
    | Dsig_value name, Tsig_value(_, _, val_desc) -> begin
        match val_desc.val_prim with
          | [] -> 
            let jtyp = generate_typ local val_desc.val_desc in
            let jinfo = generate_info_opt2 local ditem.dsig_info ditem.dsig_after_info in
            iValue name jtyp jinfo
          | primitive ->
            let jtyp = generate_typ local val_desc.val_desc in
            let jinfo = generate_info_opt2 local ditem.dsig_info ditem.dsig_after_info in
            iPrimitive name jtyp primitive jinfo
      end
    | Dsig_type(name, dkind), Tsig_type [id, _, tdecl] ->
      (* add to internal type table *)
      add_internal_reference id;

      let jparams = List.map generate_typ_param tdecl.typ_params in
      let jcstrs = 
        List.map 
          (fun (ct1, ct2, _) -> 
            (generate_typ local ct1, generate_typ local ct2))
          tdecl.typ_cstrs
      in
      let jkind = generate_type_kind local dkind tdecl.typ_kind in
      let priv = 
        match tdecl.typ_private with
            Private -> true
          | Public -> false
      in
      let jmanifest = 
        match tdecl.typ_manifest with
          | None -> None
          | Some typ -> Some (generate_typ local typ)
      in
      let jvariance = List.map generate_variance tdecl.typ_variance in
      let jinfo = generate_info_opt2 local ditem.dsig_info ditem.dsig_after_info in
      iType name jparams jcstrs jkind priv jmanifest jvariance jinfo
    | Dsig_exception name, Tsig_exception(_, _, edecl) ->
      let jargs = List.map (generate_typ local) edecl.exn_params in
      let jinfo = generate_info_opt2 local ditem.dsig_info ditem.dsig_after_info in
      iException name jargs jinfo
    | Dsig_module(name, dmty), Tsig_module(id, _, mty) -> 
      add_internal_reference id;
            
      let jmty = generate_submodule name (fun () ->
	generate_module_type local dmty mty) () in
      let jinfo = generate_info_opt2 local ditem.dsig_info ditem.dsig_after_info in
      iModule name jmty jinfo
    | Dsig_recmodule (name, dmty) , Tsig_recmodule [id,_,mty;_] -> 
      add_internal_reference id;
      
      let jmty = generate_submodule name (fun () ->
	  generate_module_type local dmty mty) () in
      let jinfo = generate_info_opt2 local ditem.dsig_info ditem.dsig_after_info in
      iModule name jmty jinfo
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
    | _, _ -> raise (Failure "generate_signature_item: Mismatch")

and generate_module_expr local dmty mexpr = 
  match dmty, mexpr.mod_desc with
    | Dmod_ident, Tmod_ident (p, _) -> {me_kind=`Ident; me_path=Some (Gentyp.path local p)}
    | _ -> raise (Failure "generate_module_expr: Mismatch")

(*************** STRUCTURES ********************)

(** Go through both tree at the same time *)
and generate_structure_item_list local dimpl_items impl_items =
  let rec loop ditems items acc =
    match ditems, items with
      | [], _ -> List.rev acc
      | _, [] -> 
	(* TODO *)
	print_endline 
	  "shouldn't go through here: _,[] > generate_structure_item_list"; 
	List.rev acc
      | {dstr_desc = Dstr_eval} :: drest , _ -> 
	(* debug *)
	print_endline "Eval found .. ?";
	loop drest items acc	
      | {dstr_desc = Dstr_open} :: drest, {str_desc = Tstr_open _} :: rest->
	loop drest rest acc	
      | ({dstr_desc = Dstr_type _} as ditem) :: drest, 
        ({str_desc = Tstr_type (tnext :: trest)} as item) :: rest -> 
        let item = {item with str_desc = Tstr_type [tnext]}
        and rest = 
          match trest with
	    | [] -> rest
            | _ -> {item with str_desc = Tstr_type trest} :: rest
        in
        let jitem = generate_structure_item local ditem item in
        loop drest rest (jitem :: acc)
      | ({dstr_desc = Dstr_recmodule _} as ditem) :: drest, 
        ({str_desc = Tstr_recmodule (mnext :: mrest)} as item) :: rest -> 
        let item = {item with str_desc = Tstr_recmodule [mnext]} 
        and rest = 
          match mrest with
	      [] -> rest
            | _ -> {item with str_desc = Tstr_recmodule mrest} :: rest
        in
        let jitem = generate_structure_item local ditem item in
        loop drest rest (jitem :: acc)
      (* A class *)
      | ({dstr_desc = Dstr_class (_, cl_expr)} as ditem) :: drest, 
        ({str_desc = Tstr_class (cnext :: crest)} as item) :: rest ->
        let item = {item with str_desc = Tstr_class [cnext]} 
        and rest = 
          match crest with
	      [] -> rest
            | _ -> {item with str_desc = Tstr_class crest} :: rest
        in
        let jitem = generate_structure_item local ditem item in
        loop drest rest (jitem :: acc)
      (* A class type *)
      | ({dstr_desc = Dstr_class_type _} as ditem) :: drest, 
        ({str_desc = Tstr_class_type (cnext :: crest)} as item) :: rest -> 
        let item = {item with str_desc = Tstr_class_type [cnext]}
        and rest = 
          match crest with
	      [] -> rest
            | _ -> {item with str_desc = Tstr_class_type crest} :: rest
        in
        let jitem = generate_structure_item local ditem item in
        loop drest rest (jitem :: acc)
      (* Comment *)
      | ({dstr_desc= Dstr_comment} as ditem) :: drest, _ ->
	let jinfo = generate_info_opt local ditem.dstr_info in
	let jitem = iComment jinfo in
	loop drest items (jitem :: acc)	

      (* Stop comment *)
      |  {dstr_desc = Dstr_stop} :: drest, _ ->
	(* TODO *)
	print_endline "Stop comment not handled";
	loop drest items acc
      (*raise (Failure "not supported")*)
	  
      | ditem :: drest, item :: rest ->
        let jitem = generate_structure_item local ditem item in
        loop drest rest (jitem :: acc)  
  in
  loop dimpl_items impl_items []

and generate_structure_item local ditem item =
  match ditem.dstr_desc, item.str_desc with
    | Dstr_value (Some name), Tstr_value (rec_flag, [(patt,_)])  ->
      let jtyp = Gentyp.type_scheme local patt.pat_type in
      let jinfo = generate_info_opt local ditem.dstr_info in
      iValue name jtyp jinfo

    | Dstr_value None, Tstr_value (rec_flag, [(patt, _)]) ->
      (* TODO: why does this happen? *)
      iComment None
    | Dstr_value _, Tstr_value (rec_flag, _) ->
      (* TODO: why does this happen? *)
      iComment None

    | Dstr_type(name, dkind), Tstr_type [id, _, tdecl] ->
      (* add to internal type table *)
      add_internal_reference id;

      let jparams = List.map generate_typ_param tdecl.typ_params in
      let jcstrs = 
        List.map 
          (fun (ct1, ct2, _) -> 
            (generate_typ local ct1, generate_typ local ct2))
          tdecl.typ_cstrs
      in
      let jkind = generate_type_kind local dkind tdecl.typ_kind in
      let priv = 
        match tdecl.typ_private with
            Private -> true
          | Public -> false
      in
      let jmanifest = 
        match tdecl.typ_manifest with
          | None -> None
          | Some typ -> Some (generate_typ local typ)
      in
      let jvariance = List.map generate_variance tdecl.typ_variance in
      let jinfo = generate_info_opt local ditem.dstr_info in
      iType name jparams jcstrs jkind priv jmanifest jvariance jinfo
    | Dstr_exception name, Tstr_exception(_, _, edecl) ->
      let jargs = List.map (generate_typ local) edecl.exn_params in
      let jinfo = generate_info_opt local ditem.dstr_info in
      iException name jargs jinfo
    | Dstr_module(name, dmexpr), Tstr_module(id, _, mexpr) ->

      add_internal_reference id;

      let jmty = generate_submodule name
	(fun () -> generate_module_str_type local dmexpr mexpr) () in
      let jinfo = generate_info_opt local ditem.dstr_info in
      iModule name jmty jinfo

    | Dstr_recmodule (name, dmty, _), Tstr_recmodule ((id,_,mty,_)::_) -> 

      add_internal_reference id;
      
      let jmty = generate_submodule name
	(fun () -> generate_module_type local dmty mty) () in
      let jinfo = generate_info_opt local ditem.dstr_info in
      iModule name jmty jinfo
	
    | Dstr_modtype(name, dmty), Tstr_modtype(id, _, mty) ->

      add_internal_reference id;

      let jmty = generate_submodule name
	(fun () -> generate_module_type local dmty mty) () in
      let jinfo = generate_info_opt local ditem.dstr_info in
      iModType name (Some jmty) jinfo
	
    | Dstr_open, Tstr_open _ -> 
     (* TODO *)
      iComment None
   (*raise (Failure "Not supported")*)
    | Dstr_include dmty, Tstr_include(mty, typ_sig) ->
      add_include_references typ_sig;
      
     let jmty = generate_module_str_type local dmty mty in
     let jinfo = generate_info_opt local ditem.dstr_info in
     let mty_type = mty.mod_type in
     let obj = iInclude jmty jinfo in
     Index.add_include_module_type obj mty_type;
     obj

   | Dstr_class(name, dclty), Tstr_class [cl_desc, _, _] -> 
     (match cl_desc with
	 { ci_id_class = id_c; ci_id_class_type = id_ct;
	   ci_id_object = id_o; ci_id_typesharp = id_t; _} -> 
	   List.iter add_internal_reference [id_c; id_ct; id_o; id_t]);

     let jparams = List.map generate_class_param (fst cl_desc.ci_params) in
     let jvariance = List.map generate_variance cl_desc.ci_variance in
     let virt = 
       match cl_desc.ci_virt with
	   Virtual -> true
	 | Concrete -> false
     in
     let jclty = generate_class_struct local dclty cl_desc.ci_expr in
     let jinfo = generate_info_opt local ditem.dstr_info in
     iClass name jparams jvariance virt jclty jinfo
   | Dstr_class_type(name, dclty), Tstr_class_type [(id, _, clty_decl)] ->
     add_internal_reference id;

     let jparams = List.map generate_class_param (fst clty_decl.ci_params) in
     let jvariance = List.map generate_variance clty_decl.ci_variance in
     let virt = 
       match clty_decl.ci_virt with
	   Virtual -> true
	 | Concrete -> false
     in
     let jclty = generate_class_type local dclty clty_decl.ci_expr in
     let jinfo = generate_info_opt local ditem.dstr_info in
     iClassType name jparams jvariance virt jclty jinfo
   | Dstr_primitive name , Tstr_primitive (_,_,val_desc) ->
     let jtyp = generate_typ local val_desc.val_desc in
     let jinfo = generate_info_opt local ditem.dstr_info in
     iPrimitive name jtyp val_desc.val_prim jinfo
   | x,y -> 
     (* TODO *)
     print_item_desc x;
     print_item_desc_t y;
     iComment (Some <:html<"structure item: Mismatch">>)
     (*raise (Failure "structure item: Mismatch")*)

(* debug *)
and print_item_desc = function 
  | Dstr_eval -> print_endline "Dstr_eval"
  | Dstr_value _ -> print_endline "Dstr_value _"
  | Dstr_primitive _ -> print_endline "Dstr_primitive _"
  | Dstr_type _ -> print_endline "Dstr_type _"
  | Dstr_exception _ -> print_endline "Dstr_exception _"
  | Dstr_exn_rebind _ -> print_endline "Dstr_exn_rebind _"
  | Dstr_module _ -> print_endline "Dstr_module _"
  | Dstr_recmodule _ -> print_endline "Dstr_recmodule _"
  | Dstr_modtype _ -> print_endline "Dstr_modtype _"
  | Dstr_open -> print_endline "Dstr_open"
  | Dstr_class _ -> print_endline "Dstr_class _"
  | Dstr_class_type _ -> print_endline "Dstr_class_type _"
  | Dstr_include _ -> print_endline "Dstr_include _"
  | Dstr_comment -> print_endline "Dstr_comment"
  | Dstr_stop -> print_endline "Dstr_stop"
and print_item_desc_t = function
  | Tstr_eval _ -> print_endline "Tstr_eval"
  | Tstr_value _ -> print_endline "Tstr_value"
  | Tstr_primitive _ -> print_endline "Tstr_primitive"
  | Tstr_type _ -> print_endline "Tstr_type"
  | Tstr_exception _ -> print_endline "Tstr_exception"
  | Tstr_exn_rebind _ -> print_endline "Tstr_exn"
  | Tstr_module _ -> print_endline "Tstr_module"
  | Tstr_recmodule _ -> print_endline "Tstr_recmodule"
  | Tstr_modtype _ -> print_endline "Tstr_modtype"
  | Tstr_open _ -> print_endline "Tstr_open"
  | Tstr_class _ -> print_endline "Tstr_class"
  | Tstr_class_type _ -> print_endline "Tstr_class_type"
  | Tstr_include _ -> print_endline "Tstr_include"


and generate_module_str_type local dmexpr mexpr =
  match dmexpr, mexpr.mod_desc with
    | Dmod_ident , Tmod_ident (p, _) -> kModTypeIdent (Gentyp.path local p)
    | Dmod_structure str, Tmod_structure tstr -> 
      let jstr = generate_structure_item_list local str tstr.str_items in
      kModTypeSig jstr
    | Dmod_functor (darg, dbase), Tmod_functor (ident, {txt=name} , arg , base) ->
      let jarg = generate_module_type local darg arg in
      let jbase = generate_module_str_type local dbase base in
      kModTypeFunctor name jarg jbase
    | Dmod_apply (dmexpr1, dmexpr2), Tmod_apply (mexpr1, mexpr2, _) ->
      let jbase =  generate_module_str_type local dmexpr1 mexpr1 in
      let jarg = generate_module_str_type local dmexpr2 mexpr2 in
      kModTypeApply jbase jarg
    | Dmod_constraint (dmexpr,dmty), Tmod_constraint (mexpr, tmty, mtconstr, coerc) ->
      (*Don't know what to really do with module constraint*)
      (*
	module Make (Z:sig type t end) : sig end with type persistent_singleton = Z.t = 
	struct
	type persistent_singleton = Z.t
	end
      *)
      
      generate_module_str_type local dmexpr mexpr
    | Dmod_unpack, Tmod_unpack (expr ,tmty) ->
      (*Don't know what to do with module unpack*)
      (* 
	 module Make_datumable5
	 (Versions : Versions)
	 (T : T)
	 (V1 : T_bin)
	 (V2 : T_bin)
	 (V3 : T_bin)
	 (V4 : T_bin)
	 (V5 : T_bin)
	 (V1_cvt : V(V1)(T).S)
	 (V2_cvt : V(V2)(T).S)
	 (V3_cvt : V(V3)(T).S)
	 (V4_cvt : V(V4)(T).S)
	 (V5_cvt : V(V5)(T).S)
	 : Datumable with type datum = T.t = *)
      kModTypeSig []
    | _, _ -> 
      print_endline "generate_module_str_type: Mismatch";
      kModTypeSig []
      (* raise (Failure "generate_module_str_type: Mismatch") *)

and generate_class_struct local dclexpr ci_expr = 
  (* Gros bugs en vue avec args_acc *)
  let rec loop local dclexpr ci_expr args_acc =
    match dclexpr, ci_expr.cl_desc with
      | Dcl_structure dcstruct, Tcl_structure {cstr_pat=patt; cstr_fields=fields} -> 
	(* removing the 'initialize' and 'class_comments' (don't know what to do) 
	   fields *)
	let dcstruct = List.filter 
	  (function dfield -> 
	    match dfield.dcf_desc with Dcf_init | Dcf_comment -> false | _ -> true)
	  dcstruct in
	let fields = List.filter 
	  (function tfield -> 
	    match tfield.cf_desc with Tcf_init _ -> false | _ -> true) fields in
	
	let fields = List.map2 (generate_class_field local)
	  dcstruct
	  (* don't reverse the fields this time? :l *)
	  fields in
	kClassSig (List.rev args_acc) fields
	  
      | Dcl_fun dcexpr, Tcl_fun (label, pattern, _, class_expr, _) -> 
	(* todo: handle the labels *)
	let arg = Gentyp.type_scheme local pattern.pat_type in
	loop local dcexpr class_expr (arg::args_acc)
	  
      | Dcl_apply dcexpr, Tcl_apply (class_expr, list) -> 
	(* Not sure... (neither does ocamldoc) *)
	loop local dcexpr class_expr args_acc  

      | Dcl_let dcexpr, Tcl_let (_, _, _, class_expr) -> 
	loop local dcexpr class_expr args_acc;
	  
      | Dcl_constr, Tcl_constraint (class_expr, None, _, _, _) -> 
	(* A Tcl_constraint contains a Tcl_ident *)
	let params, path = 
	  match class_expr.cl_desc with 
	    | Tcl_ident (path, _, co_typ_list) -> 
	      let params = List.map 
		(generate_typ local)
		co_typ_list in
	      let path = Gentyp.path local ~is_class:true path in	      
	      params, path
	    | _ -> assert false
	in
	kClassIdent (List.rev args_acc) params path
      
      | Dcl_constraint (dcexpr, dctyp), 
	Tcl_constraint (class_expr, Some ctyp, _, _, _) ->
	let cty1 = loop local dcexpr class_expr [] in
	let cty2 = generate_class_type local dctyp ctyp in
	kClassConstraint (List.rev args_acc) (cty1, cty2)

      | _,_ -> raise (Failure "generate_class_struct: Mismatch")
  in
  loop local dclexpr ci_expr []
    
and generate_class_field local dclfexpr clfexpr =
  let extract_type_and_virtual_from_kind = function
    | Tcfk_virtual co_typ -> 
      generate_typ local co_typ, true
    | Tcfk_concrete expr -> 
      Gentyp.type_scheme local expr.exp_type, false
  in
  match dclfexpr.dcf_desc, clfexpr.cf_desc with
    | Dcf_inher dcexpr, Tcf_inher (_, class_expr, _, _, _) ->
      let jtyp = generate_class_struct local dcexpr class_expr in
      let jinfo = generate_info_opt local dclfexpr.dcf_info in
      fInherit jtyp jinfo

    | Dcf_val dname, Tcf_val (_, loc, mut_f, _, cl_f_kind, _) ->
      let mut = match mut_f with | Mutable -> true | Immutable -> false in
      let jtyp,virt = extract_type_and_virtual_from_kind cl_f_kind in
      let jinfo = generate_info_opt local dclfexpr.dcf_info in
      fVal dname mut virt jtyp jinfo
	
    | Dcf_meth dname, Tcf_meth (_, _, priv_f, cl_f_kind, ovr_b) ->
      let jtyp, virt = extract_type_and_virtual_from_kind cl_f_kind in
      let priv = match priv_f with Private -> true | Public -> false in
      let jinfo = generate_info_opt local dclfexpr.dcf_info in      
      fMethod dname virt priv jtyp jinfo
	
    | Dcf_constr, Tcf_constr (co_typ1, co_typ2) ->
      (* Doesn't matter much, really -> won't be printed *)
      let jtyp1 = generate_typ local co_typ1 in
      let jtyp2 = generate_typ local co_typ2 in
      let jinfo = generate_info_opt local dclfexpr.dcf_info in
      fConstraint jtyp1 jtyp2 jinfo
    
    | Dcf_comment, x -> 
      assert false
      
    | Dcf_init, Tcf_init _       
    | Dcf_stop, _ -> fVal "Error" false false Html.nil None (* assert false *)
    | _,_ -> 
      print_endline "generate_class_field: Mismatch";
      fVal "Error" false false Html.nil None
      (* raise (Failure "generate_class_field: Mismatch") *)
    
let generate_file_from_interface local dintf intf =
  let dintf = match dintf with Dfile_intf intf -> intf 
    | _ -> raise (Invalid_argument "not an interface") in
  let jitems = generate_signature_item_list local dintf.dintf_items intf.sig_items in
  let jinfo = generate_info_opt local dintf.dintf_info in
  file jitems jinfo

let generate_file_from_structure local dimpl impl =
  let dimpl = match dimpl with Dfile_impl impl -> impl | _ -> assert false in
  let jitems = generate_structure_item_list local dimpl.dimpl_items impl.str_items in
  let jinfo = generate_info_opt local dimpl.dimpl_info in
  file jitems jinfo
