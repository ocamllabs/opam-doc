open Docjson
open Info
open Doctree
open Typedtree
open Location
open Asttypes
open Cow


open Types

let title_id_tag = "TITLE"

(* TO IMPROVE *)
let dummy_lookup kind name type_items =
  try
    (* a definition with the same name may appear : O(n).. *)
    List.hd (List.rev (List.find_all (function 
      | Sig_value (t_name, _) -> kind = `Value && name = t_name.Ident.name
      | Sig_type (t_name, _,_) -> kind = `Type && name = t_name.Ident.name
      | Sig_exception (t_name, _) -> kind = `Exc && name = t_name.Ident.name
      | Sig_module (t_name, _,_) -> kind = `Mod && name = t_name.Ident.name
      | Sig_modtype (t_name, _ ) -> kind = `ModT && name = t_name.Ident.name
      | Sig_class  (t_name, _ ,_) -> kind = `Class && name = t_name.Ident.name
      | Sig_class_type (t_name, _,_) -> kind = `ClassT && name = t_name.Ident.name) 
      type_items))
  with
      Not_found -> raise (Failure ("dummy_lookup: Not found :c")) 

(* TODO add support for references *)
let rec generate_text_element local elem =
  match elem with
    Raw s -> <:html<$str:s$>>
  | Code s -> <:html<<span class="code">$str:s$</span>&>>
  | PreCode s -> <:html<<span class="precode">$str:s$</span>&>>
  | Verbatim s -> <:html<<span class="verbatim">$str:s$</span>&>>
  | Style(sk, t) -> generate_style local sk t
  | List items -> <:html<<span class="list">$generate_list_items local items$</span>&>>
  | Enum items -> <:html<<span class="enum">$generate_list_items local items$</span>&>>
  | Newline -> <:html<<span class="newline"/>&>>
  | Block _ -> raise (Failure "To be removed")
  | Title(n, lbl, t) -> generate_title n lbl (generate_text local t)
  | Ref(rk, s, t) -> 
    <:html<<span class="reference">$str:s$</span>&>>
  | Special_ref _ -> raise (Failure "Not implemented")
  | Target _ -> raise (Failure "Not implemented")

and generate_text local text =
  List.fold_left 
    (fun acc elem -> <:html<$acc$$generate_text_element local elem$>>)
    Html.nil
    text

and generate_list_items local items =
  List.fold_left 
    (fun acc item -> <:html<$acc$<span class="item">$generate_text local item$</span>&>>)
    Html.nil
    items

and generate_style local sk t = 
  let s = 
    match sk with
    | SK_bold -> "bold"
    | SK_italic -> "italic"
    | SK_emphasize -> "emph"
    | SK_center -> "center"
    | SK_left -> "left"
    | SK_right -> "right"
    | SK_superscript -> "superscript"
    | SK_subscript -> "subscript"
    | SK_custom _ -> raise (Failure "Not implemented: Custom styles")
  in
    <:html<<span class="$str:s$">$generate_text local t$</span>&>>

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
  in <:html<$ftag id text$<br/><br/>&>>
  
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
    <:html<<span class="version">$str:s$</span>$generate_text local t$>>
  in
    List.fold_left 
      (fun acc before -> <:html<$acc$<span class="before">$gen_before before$</span>&>>)
      Html.nil
      befores

let generate_params local params = 
  let gen_param (s, t) =
    <:html<<span class="name">$str:s$</span>$generate_text local t$>>
  in
    List.fold_left 
      (fun acc param -> <:html<$acc$<span class="parameter">$gen_param param$</span>&>>)
      Html.nil
      params

let generate_raised local raised = 
  let gen_raised (s, t) =
    <:html<<span class="exception">$str:s$</span>$generate_text local t$>>
  in
    List.fold_left 
      (fun acc raised -> <:html<$acc$<span class="raised">$gen_raised raised$</span>&>>)
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
        <:html<$jinfo$<div class="since">$str:s$</div>&>>
  in
  let jinfo = 
    match info.i_before with
      | [] -> jinfo
      | befores -> 
        <:html<$jinfo$<div class="before">$generate_befores local befores$</div>&>>
  in
  let jinfo = 
    match info.i_deprecated with
      | None -> jinfo
      | Some t -> 
        <:html<$jinfo$<div class="deprecated">$generate_text local t$</div>&>>
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
        <:html<$jinfo$<div class="raised">$generate_raised local raised$</div>&>>
  in
  let jinfo = 
    match info.i_return_value with
      | None -> jinfo
      | Some t -> 
	<:html<$jinfo$<div class="return">$generate_text local t$</div>&>>
  in
  jinfo
    
let generate_info_opt local info =
  match info with
    None -> None
  | Some info -> Some (generate_info local info)

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
  <:html<$str:("'" ^ param.txt)$>>

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
  | _, _ -> raise (Failure "Mismatch")

let generate_class_type local dclty clty = raise (Failure "Not implemented: Class types")

let generate_with_constraint local (path, _, cstr) =
  let path = Gentyp.path local path in
    match cstr with
      Twith_type td -> 
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
  | _, _ -> raise (Failure "Mismatch")

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
          raise (Failure "Not supported")          
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
      | _, _ -> raise (Failure "Mismatch")
  in
    loop ditems items []

and generate_signature_item local ditem item =
  match ditem.dsig_desc, item.sig_desc with
      Dsig_value name, Tsig_value(_, _, val_desc) -> begin
        match val_desc.val_prim with
          | [] -> 
            let jtyp = generate_typ local val_desc.val_desc in
            let jinfo = generate_info_opt local ditem.dsig_info in
            iValue name jtyp jinfo
          | primitive ->
            let jtyp = generate_typ local val_desc.val_desc in
            let jinfo = generate_info_opt local ditem.dsig_info in
            iPrimitive name jtyp primitive jinfo
      end
    | Dsig_type(name, dkind), Tsig_type [_, _, tdecl] ->
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
      let jinfo = generate_info_opt local ditem.dsig_info in
      iType name jparams jcstrs jkind priv jmanifest jvariance jinfo
    | Dsig_exception name, Tsig_exception(_, _, edecl) ->
      let jargs = List.map (generate_typ local) edecl.exn_params in
      let jinfo = generate_info_opt local ditem.dsig_info in
      iException name jargs jinfo
    | Dsig_module(name, dmty), Tsig_module(_, _, mty) -> 
      let jmty = generate_module_type local dmty mty in
      let jinfo = generate_info_opt local ditem.dsig_info in
      iModule name jmty jinfo
    | Dsig_recmodule _ , Tsig_recmodule _ -> raise (Failure "Not implemented: Recursive modules")
    | Dsig_modtype(name, dmtyo), Tsig_modtype(_, _, mtydecl) ->
      let jmtyo = 
        match dmtyo, mtydecl with
            Some dmty, Tmodtype_manifest mty -> 
              Some (generate_module_type local dmty mty)
          | None, Tmodtype_abstract -> None
          | _, _ -> raise (Failure "Mismatch")
      in
      let jinfo = generate_info_opt local ditem.dsig_info in
      iModType name jmtyo jinfo
    | Dsig_open, Tsig_open _ -> raise (Failure "Not supported")
    | Dsig_include dmty, Tsig_include(mty, _) ->
      let jmty = generate_module_type local dmty mty in
      let jinfo = generate_info_opt local ditem.dsig_info in
      iInclude jmty jinfo
    | Dsig_class(name, dclty), Tsig_class [cl_desc] ->
      let jparams = List.map generate_class_param (fst cl_desc.ci_params) in
      let jvariance = List.map generate_variance cl_desc.ci_variance in
      let virt = 
        match cl_desc.ci_virt with
            Virtual -> true
          | Concrete -> false
      in
      let jclty = generate_class_type local dclty cl_desc.ci_expr in
      let jinfo = generate_info_opt local ditem.dsig_info in
      iClass name jparams jvariance virt jclty jinfo
    | Dsig_class_type(name, dclty), Tsig_class_type [clty_decl] ->
      let jparams = List.map generate_class_param (fst clty_decl.ci_params) in
      let jvariance = List.map generate_variance clty_decl.ci_variance in
      let virt = 
        match clty_decl.ci_virt with
            Virtual -> true
          | Concrete -> false
      in
      let jclty = generate_class_type local dclty clty_decl.ci_expr in
      let jinfo = generate_info_opt local ditem.dsig_info in
      iClassType name jparams jvariance virt jclty jinfo
    | _, _ -> raise (Failure "Mismatch")

and generate_module_expr local dmty mexpr = 
  match dmty, mexpr.mod_desc with
    | Dmod_ident, Tmod_ident (p, _) -> {me_kind=`Ident; me_path=Some (Gentyp.path local p)}
    | _ -> raise (Failure "mismatch")

(*************** STRUCTURES ********************)

(** Go through both tree at the same time *)
and generate_structure_item_list local dimpl_items impl_items =
  let type_items = impl_items.str_type in
  let impl_items = impl_items.str_items in

  let rec loop ditems items acc =
    match ditems, items with
      | [], _ -> List.rev acc
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
        let jitem = generate_structure_item local ditem item type_items in
        loop drest rest (jitem :: acc)
      | ({dstr_desc = Dstr_recmodule _} as ditem) :: drest, 
        ({str_desc = Tstr_recmodule (mnext :: mrest)} as item) :: rest -> 
        let item = {item with str_desc = Tstr_recmodule [mnext]} 
        and rest = 
          match mrest with
	      [] -> rest
            | _ -> {item with str_desc = Tstr_recmodule mrest} :: rest
        in
        let jitem = generate_structure_item local ditem item type_items in
        loop drest rest (jitem :: acc)
      (* A class *)
      | ({dstr_desc = Dstr_class _} as ditem) :: drest, 
        ({str_desc = Tstr_class (cnext :: crest)} as item) :: rest -> 
        let item = {item with str_desc = Tstr_class [cnext]} 
        and rest = 
          match crest with
	      [] -> rest
            | _ -> {item with str_desc = Tstr_class crest} :: rest
        in
        let jitem = generate_structure_item local ditem item type_items in
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
        let jitem = generate_structure_item local ditem item type_items in
        loop drest rest (jitem :: acc)
      (* Comment *)
      | ({dstr_desc= Dstr_comment} as ditem) :: drest, _ ->
	let jinfo = generate_info_opt local ditem.dstr_info in
	let jitem = iComment jinfo in
	loop drest items (jitem :: acc)	

      (* Stop comment *)
      |  {dstr_desc = Dstr_stop} :: drest, _ ->
	raise (Failure "not supported")
	  
      | ditem :: drest, item :: rest ->
        let jitem = generate_structure_item local ditem item type_items in
        loop drest rest (jitem :: acc)  
      | _, _ -> raise (Failure "mismatch")
  in
  loop dimpl_items impl_items []
    
and generate_structure_item local ditem item type_items =
  match ditem.dstr_desc, item.str_desc with
    | Dstr_value (Some name), Tstr_value (rec_flag, list)  ->
      (* WEIRD *)
      let v = dummy_lookup `Value name type_items in
      let jtyp =   Gentyp.type_scheme local  (match v with 
	| Sig_value (_, v_descr) -> v_descr.val_type 
	| _ -> raise (Failure "Generate.generate_structure_item -> value descr don't match the good kind"))
      in
      let jinfo = generate_info_opt local ditem.dstr_info in
      iValue name jtyp jinfo

    | Dstr_type(name, dkind), Tstr_type [_, _, tdecl] ->
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
    | Dstr_module(name, dmty), Tstr_module(_, _, mexpr) ->
      let jmty = generate_module_str_type local dmty mexpr in
      let jinfo = generate_info_opt local ditem.dstr_info in
      iModule name jmty jinfo
    | Dstr_recmodule _ , Tstr_recmodule _ -> raise (Failure "Not implemented: Recursive modules")
    | Dstr_modtype(name, dmtyo), Tstr_modtype(_, _, mtydecl) ->
      raise (Failure "Not implemented: module type") 
  (*let jmtyo = 
    match dmtyo, mtydecl with
    | Some dmty, Tmodtype_manifest mty -> 
    | Some (generate_module_type local dmty mty)
    | None, Tmodtype_abstract -> None
    | _, _ -> raise (Failure "Mismatch")
    in
    let jinfo = generate_info_opt local ditem.dstr_info in
    iModType name jmtyo jinfo
  *)
    | Dstr_open, Tstr_open _ -> raise (Failure "Not supported")
    | Dstr_include dmty, Tstr_include(mty, _) ->
      raise (Failure "Not implemented: module type") 
  (*
    let jmty = generate_module_type local dmty mty in
    let jinfo = generate_info_opt local ditem.dstr_info in
    iInclude jmty jinfo
  *)
    | Dstr_class(name, dclty), Tstr_class [cl_desc, _, _] ->
      raise (Failure "Not implemented: module type") 
  (*
    let jparams = List.map generate_class_param (fst cl_desc.ci_params) in
    let jvariance = List.map generate_variance cl_desc.ci_variance in
    let virt = 
    match cl_desc.ci_virt with
    Virtual -> true
    | Concrete -> false
    in
    let jclty = generate_class_type local dclty cl_desc.ci_expr in
    let jinfo = generate_info_opt local ditem.dstr_info in
    iClass name jparams jvariance virt jclty jinfo
  *)
    | Dstr_class_type(name, dclty), Tstr_class_type [clty_decl] ->
      raise (Failure "Not implemented: class type") 
  (*
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
  *)
    | _, _ -> raise (Failure "Mismatch")
and generate_module_str_type local dmty mexpr =
  match dmty, mexpr.mod_desc with
    | Dmod_ident , Tmod_ident (p, _) -> kModTypeIdent (Gentyp.path local p)
    | Dmod_structure str, Tmod_structure tstr -> 
      let jstr = generate_structure_item_list local str tstr in
      kModTypeSig jstr
    | Dmod_functor (darg, dbase), Tmod_functor (ident, {txt=name} , arg , base) ->
      let jarg = generate_module_type local darg arg in
      let jbase = generate_module_str_type local dbase base in
      kModTypeFunctor name jarg jbase
    | Dmod_apply (dmexpr1, dmexpr2), Tmod_apply (mexpr1, mexpr2, coerc) ->
      failwith "module apply not implemented"
    | Dmod_constraint (dmexpr,dmty), Tmod_constraint (mexpr, tmty, mtconstr, coerc) ->
      failwith "module apply not implemented"
    | Dmod_unpack, Tmod_unpack (expr ,tmty) ->
      failwith "module apply not implemented"
    | _, _ -> raise (Failure "mismatch")

let generate_file_from_interface local dintf intf =
  let dintf = match dintf with Dfile_intf intf -> intf | _ -> failwith "bla" in
  let jitems = generate_signature_item_list local dintf.dintf_items intf.sig_items in
  let jinfo = generate_info_opt local dintf.dintf_info in
  file jitems jinfo

let generate_file_from_structure local dimpl impl =
  let dimpl = match dimpl with Dfile_impl impl -> impl | _ -> failwith "bla" in
  let jitems = generate_structure_item_list local dimpl.dimpl_items impl in
  let jinfo = generate_info_opt local dimpl.dimpl_info in
  file jitems jinfo
  
