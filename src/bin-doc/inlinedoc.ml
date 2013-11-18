(***********************************************************************)
(*                                                                     *)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Analysis of inline documentation. *)
open Parsetree
open Doctree
open Lexing
open Location 
open Comments

(* Lex the comments within (optional) bound, before 
   (optional) finish and after (optional) start *)
let lex_comments ?bound ?start ?finish file =
  let (fname, contents) = file in
  let start_loc = 
    match bound, start with
      Some bound, Some start ->
        let bound = bound.loc_start in
        let start = start.loc_end in
          if start.pos_cnum > bound.pos_cnum then
            start
          else
            bound
    | None, Some start -> start.loc_end
    | Some bound, None -> bound.loc_start
    | None, None -> 
        { pos_fname = fname;
   	  pos_lnum = 1;
   	  pos_bol = 0;
   	  pos_cnum = 0; }
  in
  let finish_pos = 
    match bound, finish with
      Some bound, Some finish ->
        min
          finish.loc_start.pos_cnum 
          bound.loc_end.pos_cnum 
    | None, Some finish -> finish.loc_start.pos_cnum 
    | Some bound, None -> bound.loc_end.pos_cnum 
    | None, None -> String.length contents
  in
  let s = 
    try
      String.sub contents start_loc.pos_cnum (finish_pos - start_loc.pos_cnum)
    with Invalid_argument _ -> ""
  in
    Comments.lex start_loc s


let rec first_comment coms =
  match coms with
    [] -> None, []
  | Special(s, l) :: rest -> Some (s, l), rest
  | Simple :: rest -> first_comment rest
  | Blank_line :: rest -> first_comment rest
  | Stop :: rest -> None, rest

let rec just_after_comment coms =
  match coms with
    [] -> None, []
  | Special(s, l) :: rest -> Some (s, l), rest
  | _ :: rest -> None, rest

let just_before_comment coms =
  let rec loop coms =
    match coms with
      [] -> None, []
    | Special(s, l) :: rest -> Some (s, l), rest
    | Simple :: rest -> loop rest
    | Blank_line :: rest -> None, rest
    | Stop :: rest -> None, rest
  in
  let com, rest = loop (List.rev coms) in
    com, List.rev rest


let get_info stropt = 
  match stropt with
    None -> None
  | Some (str, loc) ->
    let lexbuf = Lexing.from_string str in
    Info_lexer.update_loc lexbuf loc;
    let info = Info_parser.info Info_lexer.main lexbuf in
      Some info


let parse_extra_comments mk_com stop coms =
  let rec loop acc coms =
    match coms with
      [] -> List.rev acc
    | Special(str, loc) :: rest -> 
      let lexbuf = Lexing.from_string str in
      Info_lexer.update_loc lexbuf loc;
      let info = Info_parser.info Info_lexer.main lexbuf in
      let com = mk_com info in
        loop (com :: acc) rest
    | Simple :: rest -> loop acc rest
    | Blank_line :: rest -> loop acc rest
    | Stop :: rest -> loop (stop :: acc) rest
  in
    loop [] coms

let compare_locs x y = 
  let (|||) i j = if i <> 0 then i else j in
  let sl = compare x.loc_start.pos_lnum y.loc_start.pos_lnum in
  let sc = compare x.loc_start.pos_cnum y.loc_start.pos_cnum in
  let el = compare x.loc_end.pos_lnum y.loc_end.pos_lnum in
  let ec = compare x.loc_end.pos_cnum y.loc_end.pos_cnum in
    sl ||| sc ||| el ||| ec

let sort_by_loc loc l =
  let il = List.mapi (fun idx x -> (x, idx)) l in
    List.stable_sort (fun (x, _) (y, _) -> compare_locs (loc x) (loc y)) il

let sort_by_idx l = 
  let il = List.stable_sort (fun (_, i) (_, j) -> compare i j) l in
    List.map fst il

let parse_type_declaration file after typ =
  match typ.ptype_kind with 
    Ptype_abstract -> Dtype_abstract, after
  | Ptype_variant cstrs ->
      let rec loop acc cstrs =
        match cstrs with
          [] -> Dtype_variant (sort_by_idx acc), after
        | ((name, _, _, _), idx) :: [] ->
            let com, after = just_after_comment after in
            let info = get_info com in
            let cstrs = sort_by_idx (((name.txt, info), idx) :: acc) in
              Dtype_variant cstrs, after
        | ((name, _, _, loc), idx) :: ((((_, _, _, next), _) :: _) as rest) ->
            let coms = lex_comments ~start:loc ~finish:next file in
            let com, _ = just_after_comment coms in
            let info = get_info com in
              loop (((name.txt, info), idx) :: acc) rest
      in
        loop [] (sort_by_loc (fun (_,_,_,loc) -> loc) cstrs)
  | Ptype_record fields ->
      let rec loop acc fields =
        match fields with
          [] -> Dtype_record (sort_by_idx acc), after
        | ((name, _, _, loc), idx) :: ([] as rest) ->
            let coms = 
              lex_comments ~bound:typ.ptype_loc ~start:loc file
            in
            let com, _ = just_after_comment coms in
            let info = get_info com in
              loop (((name.txt, info), idx) :: acc) rest
        | ((name, _, _, loc), idx) :: ((((_, _, _, next), _) :: _) as rest) ->
            let coms = 
              lex_comments ~bound:typ.ptype_loc ~start:loc ~finish:next file 
            in
            let com, _ = just_after_comment coms in
            let info = get_info com in
              loop (((name.txt, info), idx) :: acc) rest
      in
        loop [] (sort_by_loc (fun (_,_,_,loc) -> loc) fields)


let rec parse_class_type_field file before after field =
  match field.pctf_desc with
    Pctf_inher ct ->
      let ct = parse_class_type file ct in
      let com, before = just_before_comment before in
      let after_com, after = just_after_comment after in
      let info = get_info com in
      let after_info = get_info after_com in
      let field = 
        { dctf_desc = Dctf_inher ct;
          dctf_info = info;
          dctf_after_info = after_info } 
      in
        field, before, after
  | Pctf_val(name, _, _, _) ->
      let com, before = just_before_comment before in
      let after_com, after = just_after_comment after in
      let info = get_info com in
      let after_info = get_info after_com in
      let field = 
        { dctf_desc = Dctf_val name;
          dctf_info = info;
          dctf_after_info = after_info } 
      in
        field, before, after
  | Pctf_virt(name, _, _) | Pctf_meth(name, _, _) ->
      let com, before = just_before_comment before in
      let after_com, after = just_after_comment after in
      let info = get_info com in
      let after_info = get_info after_com in
      let field = 
        { dctf_desc = Dctf_meth name;
          dctf_info = info;
          dctf_after_info = after_info } 
      in
        field, before, after
  | Pctf_cstr _ -> 
      let field = 
        { dctf_desc = Dctf_cstr;
          dctf_info = None;
          dctf_after_info = None } 
      in
        field, before, after

and parse_class_type file cty =
  match cty.pcty_desc with
    Pcty_constr _ -> Dcty_constr
  | Pcty_fun(_, _, cty) -> Dcty_fun (parse_class_type file cty)
  | Pcty_signature clsig ->
      let stop = 
        { dctf_desc = Dctf_stop;
          dctf_info = None;
          dctf_after_info = None } 
      in
      let mk_com = 
        (fun info -> 
          { dctf_desc = Dctf_comment;
            dctf_info = Some info;
            dctf_after_info = None }) 
      in
      let before = 
        match clsig.pcsig_fields with
          [] -> 
            lex_comments ~bound:cty.pcty_loc file
        | field :: _ -> 
            lex_comments ~bound:cty.pcty_loc ~finish:field.pctf_loc file
      in
      let rec loop before acc fields =
        match fields with
          [] -> 
            let extra = parse_extra_comments mk_com stop before in
              Dcty_signature ((sort_by_idx acc) @ extra)
        | (field, idx) :: ([] as rest) ->
            let after = 
              lex_comments ~bound:cty.pcty_loc ~start:field.pctf_loc file
            in
            let field, before, after = 
              parse_class_type_field file before after field 
            in
            let extra = parse_extra_comments mk_com stop before in
            let extra = List.map (fun x -> (x, idx)) extra in
            let acc = extra @ (field, idx) :: acc in
              loop after acc rest
        | (field, idx) :: (((next, _) :: _) as rest) ->
            let after = 
              lex_comments 
                ~bound:cty.pcty_loc 
                ~start:field.pctf_loc 
                ~finish:next.pctf_loc 
                file
            in
            let field, before, after = 
              parse_class_type_field file before after field 
            in
            let extra = parse_extra_comments mk_com stop before in
            let extra = List.map (fun x -> (x, idx)) extra in
            let acc = extra @ (field, idx) :: acc in
              loop after acc rest
      in
        loop before [] (sort_by_loc (fun f -> f.pctf_loc) clsig.pcsig_fields)

let rec parse_class_field file before field =
  match field.pcf_desc with
    Pcf_inher(_, cl, _) ->
      let cl = parse_class_expr file cl in
      let com, before = just_before_comment before in
      let info = get_info com in
      let field = {dcf_desc = Dcf_inher cl; dcf_info = info} in
        field, before
  | Pcf_valvirt(name, _, _) | Pcf_val(name, _, _, _) ->
      let com, before = just_before_comment before in
      let info = get_info com in
      let field = {dcf_desc = Dcf_val name.txt; dcf_info = info} in
        field, before
  | Pcf_virt(name, _, _) | Pcf_meth(name, _, _, _) ->
      let com, before = just_before_comment before in
      let info = get_info com in
      let field = {dcf_desc = Dcf_meth name.txt; dcf_info = info} in
        field, before
  | Pcf_constr _ -> 
      let field = {dcf_desc = Dcf_constr; dcf_info = None} in
        field, before
  | Pcf_init _ -> 
      let field = {dcf_desc = Dcf_init; dcf_info = None} in
        field, before

and parse_class_expr file cl =
  match cl.pcl_desc with
    Pcl_constr _ -> Dcl_constr
  | Pcl_fun(_, _, _, cl) -> Dcl_fun (parse_class_expr file cl)
  | Pcl_apply(cl, _) -> Dcl_apply (parse_class_expr file cl)
  | Pcl_let(_, _, cl) -> Dcl_let (parse_class_expr file cl)
  | Pcl_constraint(cl, cty) -> 
      Dcl_constraint(parse_class_expr file cl, parse_class_type file cty)
  | Pcl_structure cstr ->
      let stop = { dcf_desc = Dcf_stop; dcf_info = None } in
      let mk_com = 
        (fun info -> { dcf_desc = Dcf_comment; dcf_info = Some info }) 
      in
      let before = 
        match cstr.pcstr_fields with
          [] -> 
            lex_comments ~bound:cl.pcl_loc file
        | field :: _ -> 
            lex_comments ~bound:cl.pcl_loc ~finish:field.pcf_loc file
      in
      let rec loop before acc fields =
        match fields with
          [] -> 
            let extra = parse_extra_comments mk_com stop before in
            Dcl_structure ((sort_by_idx acc) @ extra)
        | (field, idx) :: ([] as rest) ->
            let after = 
              lex_comments ~bound:cl.pcl_loc ~start:field.pcf_loc file
            in
            let field, before = 
              parse_class_field file before field 
            in
            let extra = parse_extra_comments mk_com stop before in
            let extra = List.map (fun x -> (x, idx)) extra in
            let acc = extra @ (field, idx) :: acc in
              loop after acc rest
        | (field, idx) :: (((next, _) :: _) as rest) ->
            let after = 
              lex_comments 
                ~bound:cl.pcl_loc 
                ~start:field.pcf_loc 
                ~finish:next.pcf_loc 
                file
            in
            let field, before = 
              parse_class_field file before field 
            in
            let extra = parse_extra_comments mk_com stop before in
            let extra = List.map (fun x -> (x, idx)) extra in
            let acc = extra @ (field, idx) :: acc in
              loop after acc rest
      in
        loop before [] (sort_by_loc (fun f -> f.pcf_loc) cstr.pcstr_fields)


let simplify_signature sg = 
  let rec loop acc items = 
    match items with
      [] -> List.rev acc
    | {psig_desc = Psig_type typs; _} :: rest -> 
        let items = 
          List.map 
            (fun (name, typ) -> 
              { psig_desc = Psig_type [name, typ];
                psig_loc = typ.ptype_loc }) 
            typs 
        in
          loop (List.rev_append items acc) rest
    | {psig_desc = Psig_recmodule mtys; _} :: rest -> 
        let items = 
          List.map 
            (fun (name, mty) -> 
              { psig_desc = Psig_recmodule [name, mty];
                psig_loc = mty.pmty_loc }) 
            mtys 
        in
          loop (List.rev_append items acc) rest
    | {psig_desc = Psig_class ctys; _} :: rest ->
        let items = 
          List.map 
            (fun cty -> 
              { psig_desc = Psig_class [cty];
                psig_loc = cty.pci_loc }) 
            ctys 
        in
          loop (List.rev_append items acc) rest
    | {psig_desc = Psig_class_type ctys; _} :: rest ->
        let items = 
          List.map 
            (fun cty -> 
              { psig_desc = Psig_class_type [cty];
                psig_loc = cty.pci_loc })
            ctys 
        in
          loop (List.rev_append items acc) rest
    | item :: rest -> loop (item :: acc) rest
  in
    loop [] sg

let simplify_structure str = 
  let rec loop acc items = 
    match items with
      [] -> List.rev acc
    | {pstr_desc = Pstr_value(rflag, binds); _} :: rest -> 
        let items = 
          List.map 
            (fun (pat, exp) -> 
              { pstr_desc = Pstr_value(rflag, [pat, exp]);
                pstr_loc = 
                  { loc_start = pat.ppat_loc.loc_start; 
                    loc_end = exp.pexp_loc.loc_end; 
                    loc_ghost = true } }) 
            binds 
        in
          loop (List.rev_append items acc) rest
    | {pstr_desc = Pstr_type typs; _} :: rest -> 
        let items = 
          List.map 
            (fun (name, typ) -> 
              { pstr_desc = Pstr_type [name, typ];
                pstr_loc = typ.ptype_loc }) 
            typs 
        in
          loop (List.rev_append items acc) rest
    | {pstr_desc = Pstr_recmodule mods; _} :: rest -> 
        let items = 
          List.map 
            (fun (name, mty, mexpr) -> 
              { pstr_desc = Pstr_recmodule [name, mty, mexpr];
                pstr_loc = 
                  { loc_start = mty.pmty_loc.loc_start; 
                    loc_end = mexpr.pmod_loc.loc_end; 
                    loc_ghost = true } }) 
            mods 
        in
          loop (List.rev_append items acc) rest
    | {pstr_desc = Pstr_class cls; _} :: rest ->
        let items = 
          List.map 
            (fun cl -> 
              { pstr_desc = Pstr_class [cl];
                pstr_loc = cl.pci_loc }) 
            cls 
        in
          loop (List.rev_append items acc) rest
    | {pstr_desc = Pstr_class_type ctys; _} :: rest ->
        let items = 
          List.map 
            (fun cty -> 
              { pstr_desc = Pstr_class_type [cty];
                pstr_loc = cty.pci_loc })
            ctys 
        in
          loop (List.rev_append items acc) rest
    | item :: rest -> loop (item :: acc) rest
  in
    loop [] str


let rec parse_signature_item file before after item =
  match item.psig_desc with
    Psig_value(name, _) ->
      let com, before = just_before_comment before in
      let after_com, after = just_after_comment after in
      let info = get_info com in
      let after_info = get_info after_com in
      let item = 
        { dsig_desc = Dsig_value name.txt;
          dsig_info = info;
          dsig_after_info = after_info } 
      in
        item, before, after
  | Psig_type [name, typ] ->
      let typ, after = parse_type_declaration file after typ in
      let com, before = just_before_comment before in
      let after_com, after = just_after_comment after in
      let info = get_info com in
      let after_info = get_info after_com in
      let item = 
        { dsig_desc = Dsig_type(name.txt, typ); 
          dsig_info = info;
          dsig_after_info = after_info } 
      in
        item, before, after
  | Psig_type _ -> assert false
  | Psig_exception(name, _) ->
      let com, before = just_before_comment before in
      let after_com, after = just_after_comment after in
      let info = get_info com in
      let after_info = get_info after_com in
      let item = 
        { dsig_desc = Dsig_exception name.txt;
          dsig_info = info;
          dsig_after_info = after_info } 
      in
        item, before, after
  | Psig_module(name, mty) ->
      let mty = parse_module_type file mty in
      let com, before = just_before_comment before in
      let after_com, after = just_after_comment after in
      let info = get_info com in
      let after_info = get_info after_com in
      let item = 
        { dsig_desc = Dsig_module(name.txt, mty); 
          dsig_info = info;
          dsig_after_info = after_info } 
      in
        item, before, after
  | Psig_recmodule [name, mty] ->
      let mty = parse_module_type file mty in
      let com, before = just_before_comment before in
      let after_com, after = just_after_comment after in
      let info = get_info com in
      let after_info = get_info after_com in
      let item = 
        { dsig_desc = Dsig_recmodule(name.txt, mty); 
          dsig_info = info;
          dsig_after_info = after_info } 
      in
        item, before, after
  | Psig_recmodule _ -> assert false
  | Psig_modtype(name, mtyd) ->
      let mtyo =
        match mtyd with
          Pmodtype_abstract -> None
        | Pmodtype_manifest mty -> Some (parse_module_type file mty)
      in
      let com, before = just_before_comment before in
      let after_com, after = just_after_comment after in
      let info = get_info com in
      let after_info = get_info after_com in
      let item = 
        { dsig_desc = Dsig_modtype(name.txt, mtyo); 
          dsig_info = info;
          dsig_after_info = after_info } 
      in
        item, before, after
  | Psig_open _ -> 
      let item = 
        { dsig_desc = Dsig_open;
          dsig_info = None;
          dsig_after_info = None }
      in
        item, before, after
  | Psig_include mty ->
      let mty = parse_module_type file mty in
      let com, before = just_before_comment before in
      let after_com, after = just_after_comment after in
      let info = get_info com in
      let after_info = get_info after_com in
      let item = 
        { dsig_desc = Dsig_include mty;
          dsig_info = info;
          dsig_after_info = after_info } 
      in
        item, before, after
  | Psig_class [cty] ->
      let name = cty.pci_name.txt in
      let cty = parse_class_type file cty.pci_expr in
      let com, before = just_before_comment before in
      let after_com, after = just_after_comment after in
      let info = get_info com in
      let after_info = get_info after_com in
      let item = 
        { dsig_desc = Dsig_class(name, cty); 
          dsig_info = info;
          dsig_after_info = after_info } 
      in
        item, before, after
  | Psig_class _ -> assert false
  | Psig_class_type [cty] ->
      let name = cty.pci_name.txt in
      let cty = parse_class_type file cty.pci_expr in
      let com, before = just_before_comment before in
      let after_com, after = just_after_comment after in
      let info = get_info com in
      let after_info = get_info after_com in
      let item = 
        { dsig_desc = Dsig_class_type(name, cty); 
          dsig_info = info;
          dsig_after_info = after_info } 
      in
        item, before, after
  | Psig_class_type _ -> assert false

and parse_module_type file mty =
  match mty.pmty_desc with
    Pmty_ident _ -> Dmty_ident
  | Pmty_functor(_, mty1, mty2) -> 
      Dmty_functor(parse_module_type file mty1, parse_module_type file mty2)
  | Pmty_with(mty, _) -> Dmty_with (parse_module_type file mty)
  | Pmty_typeof mexpr -> Dmty_typeof (parse_module_expr file mexpr) 
  | Pmty_signature sg ->
      let items = simplify_signature sg in
      let stop = 
        { dsig_desc = Dsig_stop;
          dsig_info = None;
          dsig_after_info = None }
      in
      let mk_com = 
        (fun info -> 
          { dsig_desc = Dsig_comment;
            dsig_info = Some info;
            dsig_after_info = None }) 
      in
      let before = 
        match items with
          [] -> 
            lex_comments ~bound:mty.pmty_loc file
        | item :: _ -> 
            lex_comments ~bound:mty.pmty_loc ~finish:item.psig_loc file
      in
      let rec loop before acc items =
        match items with
          [] -> 
            let extra = parse_extra_comments mk_com stop before in
            Dmty_signature ((sort_by_idx acc) @ extra)
        | (item, idx) :: ([] as rest) ->
            let after = 
              lex_comments ~bound:mty.pmty_loc ~start:item.psig_loc file
            in
            let item, before, after = 
              parse_signature_item file before after item 
            in
            let extra = parse_extra_comments mk_com stop before in
            let extra = List.map (fun x -> (x, idx)) extra in
            let acc = extra @ (item, idx) :: acc in
              loop after acc rest
        | (item, idx) :: (((next, _) :: _) as rest) ->
            let after = 
              lex_comments 
                ~bound:mty.pmty_loc 
                ~start:item.psig_loc 
                ~finish:next.psig_loc 
                file
            in
            let item, before, after = 
              parse_signature_item file before after item 
            in
            let extra = parse_extra_comments mk_com stop before in
            let extra = List.map (fun x -> (x, idx)) extra in
            let acc = extra @ (item, idx) :: acc in
              loop after acc rest
      in
        loop before [] (sort_by_loc (fun i -> i.psig_loc) items)

and parse_structure_item file before after item =
  match item.pstr_desc with
    Pstr_eval _ -> 
      let item = {dstr_desc = Dstr_eval; dstr_info = None} in
        item, before, after
  | Pstr_value(_, [pat, _]) -> begin
      let rec find_name = function
          Ppat_any -> None
        | Ppat_var name -> Some name
        | Ppat_tuple _ -> None
        | Ppat_constraint (pat, _) -> find_name pat.ppat_desc
        | _ -> None
      in
        match (find_name pat.ppat_desc) with
          None -> 
            let item = {dstr_desc = Dstr_value None; dstr_info = None} in
              item, before, after
        | Some name ->
            let com, before = just_before_comment before in
            let info = get_info com in
            let item = 
              { dstr_desc = Dstr_value (Some name.txt);
                dstr_info = info } 
            in
              item, before, after
    end
  | Pstr_value _ -> assert false
  | Pstr_primitive(name, _) ->
      let com, before = just_before_comment before in
      let info = get_info com in
      let item = {dstr_desc = Dstr_primitive name.txt; dstr_info = info} in
        item, before, after
  | Pstr_type [name, typ] ->
      let typ, after = parse_type_declaration file after typ in
      let com, before = just_before_comment before in
      let info = get_info com in
      let item = 
        { dstr_desc = Dstr_type(name.txt, typ); 
          dstr_info = info } 
      in
        item, before, after
  | Pstr_type _ -> assert false
  | Pstr_exception(name, _) ->
      let com, before = just_before_comment before in
      let info = get_info com in
      let item = {dstr_desc = Dstr_exception name.txt; dstr_info = info} in
        item, before, after
  | Pstr_exn_rebind(name, _) ->
      let com, before = just_before_comment before in
      let info = get_info com in
      let item = {dstr_desc = Dstr_exn_rebind name.txt; dstr_info = info} in
        item, before, after
  | Pstr_module(name, mexpr) ->
      let mexpr = parse_module_expr file mexpr in
      let com, before = just_before_comment before in
      let info = get_info com in
      let item = 
        { dstr_desc = Dstr_module(name.txt, mexpr); 
          dstr_info = info } 
      in
        item, before, after
  | Pstr_recmodule [name, mty, mexpr] ->
      let mty = parse_module_type file mty in
      let mexpr = parse_module_expr file mexpr in
      let com, before = just_before_comment before in
      let info = get_info com in
      let item = 
        { dstr_desc = Dstr_recmodule(name.txt, mty, mexpr); 
          dstr_info = info } 
      in
        item, before, after
  | Pstr_recmodule _ -> assert false
  | Pstr_modtype(name, mty) ->
      let mty = parse_module_type file mty in
      let com, before = just_before_comment before in
      let info = get_info com in
      let item = 
        { dstr_desc = Dstr_modtype(name.txt, mty); 
          dstr_info = info } 
      in
        item, before, after
  | Pstr_open _ -> 
      let item = {dstr_desc = Dstr_open; dstr_info = None} in
        item, before, after
  | Pstr_include mexpr ->
      let mexpr = parse_module_expr file mexpr in
      let com, before = just_before_comment before in
      let info = get_info com in
      let item = {dstr_desc = Dstr_include mexpr; dstr_info = info} in
        item, before, after
  | Pstr_class [cl] ->
      let name = cl.pci_name.txt in
      let cl = parse_class_expr file cl.pci_expr in
      let com, before = just_before_comment before in
      let info = get_info com in
      let item = 
        { dstr_desc = Dstr_class(name, cl); 
          dstr_info = info } 
      in
        item, before, after
  | Pstr_class _ -> assert false
  | Pstr_class_type [cty] ->
      let name = cty.pci_name.txt in
      let cty = parse_class_type file cty.pci_expr in
      let com, before = just_before_comment before in
      let info = get_info com in
      let item = 
        { dstr_desc = Dstr_class_type(name, cty); 
          dstr_info = info} 
      in
        item, before, after
  | Pstr_class_type _ -> assert false

and parse_module_expr file mexpr =
  match mexpr.pmod_desc with
    Pmod_ident _ -> Dmod_ident
  | Pmod_functor(_, mty, mexpr) -> 
      Dmod_functor(parse_module_type file mty, parse_module_expr file mexpr)
  | Pmod_apply(mexpr1, mexpr2) -> 
      Dmod_apply(parse_module_expr file mexpr1, parse_module_expr file mexpr2)
  | Pmod_constraint(mexpr, mty) -> 
      Dmod_constraint(parse_module_expr file mexpr, parse_module_type file mty)
  | Pmod_unpack _ -> Dmod_unpack
  | Pmod_structure str ->
      let items = simplify_structure str in
      let stop = { dstr_desc = Dstr_stop; dstr_info = None } in
      let mk_com = 
        (fun info -> { dstr_desc = Dstr_comment; dstr_info = Some info }) 
      in
      let before = 
        match items with
          [] -> 
            lex_comments ~bound:mexpr.pmod_loc file
        | item :: _ -> 
            lex_comments ~bound:mexpr.pmod_loc ~finish:item.pstr_loc file
      in
      let rec loop before acc items =
        match items with
          [] -> 
            let extra = parse_extra_comments mk_com stop before in
            Dmod_structure ((sort_by_idx acc) @ extra)
        | (item, idx) :: ([] as rest) ->
            let after = 
              lex_comments ~bound:mexpr.pmod_loc ~start:item.pstr_loc file
            in
            let item, before, after = 
              parse_structure_item file before after item 
            in
            let extra = parse_extra_comments mk_com stop before in
            let extra = List.map (fun x -> (x, idx)) extra in
            let acc = extra @ (item, idx) :: acc in
              loop after acc rest
        | (item, idx) :: (((next, _) :: _) as rest) ->
            let after = 
              lex_comments 
                ~bound:mexpr.pmod_loc 
                ~start:item.pstr_loc 
                ~finish:next.pstr_loc 
                file
            in
            let item, before, after = 
              parse_structure_item file before after item 
            in
            let extra = parse_extra_comments mk_com stop before in
            let extra = List.map (fun x -> (x, idx)) extra in
            let acc = extra @ (item, idx) :: acc in
              loop after acc rest
      in
        loop before [] (sort_by_loc (fun i -> i.pstr_loc) items)

let parse_interface file sg = 
  let items = simplify_signature sg in
  let stop = 
    { dsig_desc = Dsig_stop;
      dsig_info = None;
      dsig_after_info = None }
  in
  let mk_com = 
    (fun info -> 
      { dsig_desc = Dsig_comment;
        dsig_info = Some info;
        dsig_after_info = None }) 
  in
  let before = 
    match items with
      [] -> 
        lex_comments file
    | item :: _ -> 
        lex_comments ~finish:item.psig_loc file
  in
  let com, before = first_comment before in
  let info = get_info com in
  let rec loop before acc items =
    match items with
      [] -> 
        let extra = parse_extra_comments mk_com stop before in
          (sort_by_idx acc) @ extra
    | (item, idx) :: ([] as rest) ->
      let after = 
        lex_comments ~start:item.psig_loc file
      in
      let item, before, after = 
        parse_signature_item file before after item 
      in
      let extra = parse_extra_comments mk_com stop before in
      let extra = List.map (fun x -> (x, idx)) extra in
      let acc = extra @ (item, idx) :: acc in
        loop after acc rest
    | (item, idx) :: (((next, _) :: _) as rest) ->
      let after = 
        lex_comments ~start:item.psig_loc ~finish:next.psig_loc file
      in
      let item, before, after = 
        parse_signature_item file before after item 
      in
      let extra = parse_extra_comments mk_com stop before in
      let extra = List.map (fun x -> (x, idx)) extra in
      let acc = extra @ (item, idx) :: acc in
        loop after acc rest
  in
  let items = loop before [] (sort_by_loc (fun i -> i.psig_loc) items) in
    { dintf_items = items;
      dintf_info = info; }

let parse_implementation file str = 
  let items = simplify_structure str in
  let stop = { dstr_desc = Dstr_stop; dstr_info = None } in
  let mk_com = 
    (fun info -> { dstr_desc = Dstr_comment; dstr_info = Some info }) 
  in
  let before = 
    match items with
      [] -> 
        lex_comments file
    | item :: _ -> 
      lex_comments ~finish:item.pstr_loc file
  in
  let com, before = first_comment before in
  let info = get_info com in
  let rec loop before acc items =
    match items with
      [] -> 
        let extra = parse_extra_comments mk_com stop before in
          (sort_by_idx acc) @ extra
    | (item, idx) :: ([] as rest) ->
        let after = 
          lex_comments ~start:item.pstr_loc file
        in
        let item, before, after = 
          parse_structure_item file before after item 
        in
        let extra = parse_extra_comments mk_com stop before in
        let extra = List.map (fun x -> (x, idx)) extra in
        let acc = extra @ (item, idx) :: acc in
          loop after acc rest
    | (item, idx) :: (((next, _) :: _) as rest) ->
        let after = 
          lex_comments 
            ~start:item.pstr_loc 
            ~finish:next.pstr_loc 
            file
        in
        let item, before, after = 
          parse_structure_item file before after item 
        in
        let extra = parse_extra_comments mk_com stop before in
        let extra = List.map (fun x -> (x, idx)) extra in
        let acc = extra @ (item, idx) :: acc in
          loop after acc rest
  in
  let items = loop before [] (sort_by_loc (fun i -> i.pstr_loc) items) in
    { dimpl_items = items;
      dimpl_info = info; }
 
