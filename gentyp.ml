(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*         Xavier Leroy, Jerome Vouillon, Daniel de Rauglaudre         *)
(*               projet Cristal, INRIA Rocquencourt                    *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Format
open Types
open Btype
open Ctype

type out_ident =
  | Oide_apply of out_ident * out_ident
  | Oide_dot of out_ident * string
  | Oide_internal_ident of string * Ident.t * bool (* is_class ? *)
  | Oide_external_ident of string * bool (* is_class ? *)

type out_type =
  | Otyp_alias of out_type * string
  | Otyp_arrow of string * out_type * out_type
  | Otyp_class of bool * out_ident * out_type list
  | Otyp_constr of out_ident * out_type list
  | Otyp_object of (string * out_type) list * bool option
  | Otyp_stuff of string
  | Otyp_tuple of out_type list
  | Otyp_var of bool * string
  | Otyp_variant of
      bool * out_variant * bool * (string list) option
  | Otyp_poly of string list * out_type
  | Otyp_module of string * string list * out_type list

and out_variant =
  | Ovar_fields of (string * bool * out_type list) list
  | Ovar_name of out_ident * out_type list

let index = ref None

(* Added semantic tags to identifiers and special handling of pervasives *)
let rec print_ident ppf id = 
  
  let rec loop (elems:string list) = function
    (* lib externe, index a interrogé *)
    | Oide_external_ident (name, is_class) ->
      (* A.B.c *)
      (* elems : contient ["B";"c"] *) 
      
      let html_path =
	try
	  let local = match !index with 
	    | Some local -> local 
	    | None -> raise Not_found in
	  Some (Index.local_lookup local (name::elems))
	with 
	    Not_found -> 
	      (*(* debug *)
	      Printf.eprintf "Reference to %s : unresolved\n%!" 
		(String.concat "." (name::elems));*)
	      None
      in
      let concrete_name = String.concat "."
	(if name = "Pervasives" && !(Opam_doc_config.filter_pervasives) then
	    elems 
	 else name::elems)
      in
      begin
	match html_path with
	  | Some path ->
	    fprintf ppf "@{<path:%s>%s@}" path concrete_name
	  | None ->
	    fprintf ppf "@{<unresolved>%s@}" concrete_name
      end
	
    | Oide_internal_ident (name, id, is_class) ->
      begin
	  try
	    (* Looking up in the internal reference base *)
	    let module_list = Index.lookup_internal_reference id in
	    if name.[0] >= 'A' && name.[0] <= 'Z' then
	      let base_path = String.concat "." module_list in
	      let html_path =
		if List.length elems = 0 then 
		  "?package="^ !Opam_doc_config.current_package
		  ^ "&module="^base_path^"."^name
		else
		  let res, last_item = 
		    let rev = List.rev elems in List.rev (List.tl rev), List.hd rev in
		  "?package="^ !Opam_doc_config.current_package
		  ^"&module="^base_path
		  ^"."
		  ^name
		  ^(List.fold_left (fun acc s -> acc^"."^s) "" res)
		  ^"&type="^last_item 
	      in
	      fprintf ppf "@{<path:%s>%s@}" html_path (String.concat "." (name::elems))
	    else
	      let html_path = 
		"?package=" ^ !Opam_doc_config.current_package 
		^"&module=" ^ (String.concat "." (elems@module_list))
		^"&type="^name in
	      fprintf ppf "@{<path:%s>%s@}" html_path (String.concat "." (name::elems))
	  with 
	      Not_found -> 
		(*(* debug *)
		Printf.eprintf "Reference to internal %s : unresolved - stamp : %d\n%!" 
		  name id.Ident.stamp;*)
		fprintf ppf "@{<unresolved>%s@}" (String.concat "." (name::elems))
	end	
    | Oide_dot (sub_id, name) ->
      loop (name::elems) sub_id 
    | Oide_apply (id1, id2) ->
      fprintf ppf "%a(%a)" print_ident id2 print_ident id2
  in

  loop [] id

(* Types *)

let rec print_list pr sep ppf =
  function
  [] -> ()
    | [a] -> pr ppf a
    | a :: l -> pr ppf a; sep ppf; print_list pr sep ppf l

let pr_present =
  print_list (fun ppf s -> fprintf ppf "`%s" s) (fun ppf -> fprintf ppf "@ ")

let pr_vars =
  print_list (fun ppf s -> fprintf ppf "'%s" s) (fun ppf -> fprintf ppf "@ ")

let rec print_out_type ppf =
  function
    | Otyp_alias (ty, s) ->
      fprintf ppf "@[%a@ as '%s@]" print_out_type ty s
    | Otyp_poly (sl, ty) ->
      fprintf ppf "@[<hov 2>%a.@ %a@]"
        pr_vars sl
        print_out_type ty
    | ty ->
      print_out_type_1 ppf ty

and print_out_type_1 ppf =
  function
  Otyp_arrow (lab, ty1, ty2) ->
    pp_open_box ppf 0;
    if lab <> "" then (pp_print_string ppf lab; pp_print_char ppf ':');
    print_out_type_2 ppf ty1;
    pp_print_string ppf " ->";
    pp_print_space ppf ();
    print_out_type_1 ppf ty2;
    pp_close_box ppf ()
    | ty -> print_out_type_2 ppf ty
and print_out_type_2 ppf =
  function
  Otyp_tuple tyl ->
    fprintf ppf "@[<0>%a@]" (print_typlist print_simple_out_type " *") tyl
    | ty -> print_simple_out_type ppf ty
and print_simple_out_type ppf =
  function
  Otyp_class (ng, id, tyl) ->
    fprintf ppf "@[%a%s#%a@]" print_typargs tyl (if ng then "_" else "")
      print_ident id
    | Otyp_constr (id, tyl) ->
      pp_open_box ppf 0;
      print_typargs ppf tyl;
      print_ident ppf id;
      pp_close_box ppf ()
    | Otyp_object (fields, rest) ->
      fprintf ppf "@[<2>< %a >@]" (print_fields rest) fields
    | Otyp_stuff s -> pp_print_string ppf s
    | Otyp_var (ng, s) -> fprintf ppf "'%s%s" (if ng then "_" else "") s
    | Otyp_variant (non_gen, row_fields, closed, tags) ->
      let print_present ppf =
        function
        None | Some [] -> ()
          | Some l -> fprintf ppf "@;<1 -2>> @[<hov>%a@]" pr_present l
      in
      let print_fields ppf =
        function
        Ovar_fields fields ->
          print_list print_row_field (fun ppf -> fprintf ppf "@;<1 -2>| ")
            ppf fields
          | Ovar_name (id, tyl) ->
            fprintf ppf "@[%a%a@]" print_typargs tyl print_ident id
      in
      fprintf ppf "%s[%s@[<hv>@[<hv>%a@]%a ]@]" (if non_gen then "_" else "")
        (if closed then if tags = None then " " else "< "
         else if tags = None then "> " else "? ")
        print_fields row_fields
        print_present tags
    | Otyp_alias _ | Otyp_poly _ | Otyp_arrow _ | Otyp_tuple _ as ty ->
      pp_open_box ppf 1;
      pp_print_char ppf '(';
      print_out_type ppf ty;
      pp_print_char ppf ')';
      pp_close_box ppf ()
    | Otyp_module (p, n, tyl) ->
      fprintf ppf "@[<1>(module %s" p;
      let first = ref true in
      List.iter2
        (fun s t ->
          let sep = if !first then (first := false; "with") else "and" in
          fprintf ppf " %s type %s = %a" sep s print_out_type t
        )
        n tyl;
      fprintf ppf ")@]"
and print_fields rest ppf =
  function
  [] ->
    begin match rest with
        Some non_gen -> fprintf ppf "%s.." (if non_gen then "_" else "")
      | None -> ()
    end
    | [s, t] ->
      fprintf ppf "%s : %a" s print_out_type t;
      begin match rest with
          Some _ -> fprintf ppf ";@ "
	| None -> ()
      end;
      print_fields rest ppf []
    | (s, t) :: l ->
      fprintf ppf "%s : %a;@ %a" s print_out_type t (print_fields rest) l
and print_row_field ppf (l, opt_amp, tyl) =
  let pr_of ppf =
    if opt_amp then fprintf ppf " of@ &@ "
    else if tyl <> [] then fprintf ppf " of@ "
    else fprintf ppf ""
  in
  fprintf ppf "@[<hv 2>`%s%t%a@]" l pr_of (print_typlist print_out_type " &")
    tyl
and print_typlist print_elem sep ppf =
  function
  [] -> ()
    | [ty] -> print_elem ppf ty
    | ty :: tyl ->
      print_elem ppf ty;
      pp_print_string ppf sep;
      pp_print_space ppf ();
      print_typlist print_elem sep ppf tyl
and print_typargs ppf =
  function
  [] -> ()
    | [ty1] -> print_simple_out_type ppf ty1; pp_print_space ppf ()
    | tyl ->
      pp_open_box ppf 1;
      pp_print_char ppf '(';
      print_typlist print_out_type "," ppf tyl;
      pp_print_char ppf ')';
      pp_close_box ppf ();
      pp_print_space ppf ()

(* Print a path *)

(* Removed special handling of pervasives *)
let rec tree_of_path ?(is_class=false) p = 
  match p with (*function*)
  | Path.Pident id ->
    let pers = Ident.persistent id in
    let name = Ident.name id in
    if pers then
      Oide_external_ident (name, is_class)
    else
      Oide_internal_ident (name, id, is_class)
  | Path.Pdot(p, s, pos) ->
    Oide_dot (tree_of_path ~is_class:is_class p, s)
  | Path.Papply(p1, p2) ->
    Oide_apply (tree_of_path ~is_class:is_class p1, tree_of_path ~is_class:is_class p2)
      
let path ppf ?(is_class=false) p =
  print_ident ppf (tree_of_path ~is_class:is_class p)

(* Print a type expression *)

let names = ref ([] : (type_expr * string) list)
let name_counter = ref 0
let named_vars = ref ([] : string list)

let reset_names () = names := []; name_counter := 0; named_vars := []
let add_named_var ty =
  match ty.desc with
      Tvar (Some name) | Tunivar (Some name) ->
	if List.mem name !named_vars then () else
	  named_vars := name :: !named_vars
    | _ -> ()

let rec new_name () =
  let name =
    if !name_counter < 26
    then String.make 1 (Char.chr(97 + !name_counter))
    else String.make 1 (Char.chr(97 + !name_counter mod 26)) ^
      string_of_int(!name_counter / 26) in
  incr name_counter;
  if List.mem name !named_vars
    || List.exists (fun (_, name') -> name = name') !names
  then new_name ()
  else name

let name_of_type t =
  (* We've already been through repr at this stage, so t is our representative
     of the union-find class. *)
  try List.assq t !names with Not_found ->
    let name =
      match t.desc with
          Tvar (Some name) | Tunivar (Some name) ->
          (* Some part of the type we've already printed has assigned another
           * unification variable to that name. We want to keep the name, so try
           * adding a number until we find a name that's not taken. *)
            let current_name = ref name in
            let i = ref 0 in
            while List.exists (fun (_, name') -> !current_name = name') !names do
              current_name := name ^ (string_of_int !i);
              i := !i + 1;
            done;
            !current_name
	| _ ->
          (* No name available, create a new one *)
          new_name ()
    in
    (* Exception for type declarations *)
    if name <> "_" then names := (t, name) :: !names;
    name

let check_name_of_type t = ignore(name_of_type t)

let remove_names tyl =
  let tyl = List.map repr tyl in
  names := List.filter (fun (ty,_) -> not (List.memq ty tyl)) !names

let visited_objects = ref ([] : type_expr list)
let aliased = ref ([] : type_expr list)
let delayed = ref ([] : type_expr list)

let add_delayed t =
  if not (List.memq t !delayed) then delayed := t :: !delayed

let is_aliased ty = List.memq (proxy ty) !aliased
let add_alias ty =
  let px = proxy ty in
  if not (is_aliased px) then begin
    aliased := px :: !aliased;
    add_named_var px
  end

let aliasable ty =
  match ty.desc with Tvar _ | Tunivar _ | Tpoly _ -> false | _ -> true

let namable_row row =
  row.row_name <> None &&
    List.for_all
    (fun (_, f) ->
      match row_field_repr f with
	| Reither(c, l, _, _) ->
          row.row_closed && if c then l = [] else List.length l = 1
	| _ -> true)
    row.row_fields

let rec mark_loops_rec visited ty =
  let ty = repr ty in
  let px = proxy ty in
  if List.memq px visited && aliasable ty then add_alias px else
    let visited = px :: visited in
    match ty.desc with
      | Tvar _ -> add_named_var ty
      | Tarrow(_, ty1, ty2, _) ->
        mark_loops_rec visited ty1; mark_loops_rec visited ty2
      | Ttuple tyl -> List.iter (mark_loops_rec visited) tyl
      | Tconstr(_, tyl, _) | Tpackage (_, _, tyl) ->
        List.iter (mark_loops_rec visited) tyl
      | Tvariant row ->
        if List.memq px !visited_objects then add_alias px else
          begin
            let row = row_repr row in
            if not (static_row row) then
              visited_objects := px :: !visited_objects;
            match row.row_name with
              | Some(p, tyl) when namable_row row ->
		List.iter (mark_loops_rec visited) tyl
              | _ ->
		iter_row (mark_loops_rec visited) row
          end
      | Tobject (fi, nm) ->
	if List.memq px !visited_objects then add_alias px else
          begin
            if opened_object ty then
              visited_objects := px :: !visited_objects;
            begin match !nm with
              | None ->
		let fields, _ = flatten_fields fi in
		List.iter
                  (fun (_, kind, ty) ->
                    if field_kind_repr kind = Fpresent then
                      mark_loops_rec visited ty)
                  fields
              | Some (_, l) ->
		List.iter (mark_loops_rec visited) (List.tl l)
            end
          end
      | Tfield(_, kind, ty1, ty2) when field_kind_repr kind = Fpresent ->
	mark_loops_rec visited ty1; mark_loops_rec visited ty2
      | Tfield(_, _, _, ty2) ->
	mark_loops_rec visited ty2
      | Tnil -> ()
      | Tsubst ty -> mark_loops_rec visited ty
      | Tlink _ -> assert false
      | Tpoly (ty, tyl) ->
	List.iter (fun t -> add_alias t) tyl;
	mark_loops_rec visited ty
      | Tunivar _ -> add_named_var ty

let mark_loops ty =
  normalize_type Env.empty ty;
  mark_loops_rec [] ty;;

let reset_loop_marks () =
  visited_objects := []; aliased := []; delayed := []

let reset () =
  reset_names (); reset_loop_marks ()

let reset_and_mark_loops ty =
  reset (); mark_loops ty

let rec tree_of_typexp sch ty =
  let ty = repr ty in
  let px = proxy ty in
  if List.mem_assq px !names && not (List.memq px !delayed) then
    let mark = is_non_gen sch ty in
    Otyp_var (mark, name_of_type px) else

    let pr_typ () =
      match ty.desc with
	| Tvar _ ->
          Otyp_var (is_non_gen sch ty, name_of_type ty)
	| Tarrow(l, ty1, ty2, _) ->
          let pr_arrow lab ty1 ty2 =
            let t1 =
              if is_optional lab then
		match (repr ty1).desc with
		  | Tconstr(path, [ty], _)
                      when Path.same path Predef.path_option ->
                    tree_of_typexp sch ty
		  | _ -> Otyp_stuff "<hidden>"
              else tree_of_typexp sch ty1 in
            Otyp_arrow (lab, t1, tree_of_typexp sch ty2) in
          pr_arrow l ty1 ty2
	| Ttuple tyl ->
          Otyp_tuple (tree_of_typlist sch tyl)
	| Tconstr(p, tyl, abbrev) ->
          Otyp_constr (tree_of_path p, tree_of_typlist sch tyl)
	| Tvariant row ->
          let row = row_repr row in
          let fields =
            if row.row_closed then
              List.filter (fun (_, f) -> row_field_repr f <> Rabsent)
		row.row_fields
            else row.row_fields in
          let present =
            List.filter
              (fun (_, f) ->
		match row_field_repr f with
		  | Rpresent _ -> true
		  | _ -> false)
              fields in
          let all_present = List.length present = List.length fields in
          begin match row.row_name with
            | Some(p, tyl) when namable_row row ->
              let id = tree_of_path p in
              let args = tree_of_typlist sch tyl in
              if row.row_closed && all_present then
		Otyp_constr (id, args)
              else
		let non_gen = is_non_gen sch px in
		let tags =
                  if all_present then None else Some (List.map fst present) in
		Otyp_variant (non_gen, Ovar_name(tree_of_path p, args),
                              row.row_closed, tags)
            | _ ->
              let non_gen =
		not (row.row_closed && all_present) && is_non_gen sch px in
              let fields = List.map (tree_of_row_field sch) fields in
              let tags =
		if all_present then None else Some (List.map fst present) in
              Otyp_variant (non_gen, Ovar_fields fields, row.row_closed, tags)
          end
	| Tobject (fi, nm) ->
          tree_of_typobject sch fi !nm
	| Tnil | Tfield _ ->
          tree_of_typobject sch ty None
	| Tsubst ty ->
          tree_of_typexp sch ty
	| Tlink _ ->
          assert false
	| Tpoly (ty, []) ->
          tree_of_typexp sch ty
	| Tpoly (ty, tyl) ->
        (*let print_names () =
          List.iter (fun (_, name) -> prerr_string (name ^ " ")) !names;
          prerr_string "; " in *)
          let tyl = List.map repr tyl in
          if tyl = [] then tree_of_typexp sch ty else begin
            let old_delayed = !delayed in
          (* Make the names delayed, so that the real type is
             printed once when used as proxy *)
            List.iter add_delayed tyl;
            let tl = List.map name_of_type tyl in
            let tr = Otyp_poly (tl, tree_of_typexp sch ty) in
          (* Forget names when we leave scope *)
            remove_names tyl;
            delayed := old_delayed; tr
          end
	| Tunivar _ ->
	  Otyp_var (false, name_of_type ty)
	| Tpackage (p, n, tyl) ->
	  let n =
            List.map (fun li -> String.concat "." (Longident.flatten li)) n in
	  Otyp_module (Path.name p, n, tree_of_typlist sch tyl)
    in
    if List.memq px !delayed then delayed := List.filter ((!=) px) !delayed;
    if is_aliased px && aliasable ty then begin
      check_name_of_type px;
      Otyp_alias (pr_typ (), name_of_type px) end
    else pr_typ ()

and tree_of_row_field sch (l, f) =
  match row_field_repr f with
    | Rpresent None | Reither(true, [], _, _) -> (l, false, [])
    | Rpresent(Some ty) -> (l, false, [tree_of_typexp sch ty])
    | Reither(c, tyl, _, _) ->
      if c (* contradiction: un constructeur constant qui a un argument *)
      then (l, true, tree_of_typlist sch tyl)
      else (l, false, tree_of_typlist sch tyl)
    | Rabsent -> (l, false, [] (* une erreur, en fait *))

and tree_of_typlist sch tyl =
  List.map (tree_of_typexp sch) tyl
    
and tree_of_typobject sch fi nm =
  begin match nm with
    | None ->
      let pr_fields fi =
        let (fields, rest) = flatten_fields fi in
        let present_fields =
          List.fold_right
            (fun (n, k, t) l ->
              match field_kind_repr k with
		| Fpresent -> (n, t) :: l
		| _ -> l)
            fields [] in
        let sorted_fields =
          List.sort (fun (n, _) (n', _) -> compare n n') present_fields in
        tree_of_typfields sch rest sorted_fields in
      let (fields, rest) = pr_fields fi in
      Otyp_object (fields, rest)
    | Some (p, ty :: tyl) ->
      let non_gen = is_non_gen sch (repr ty) in
      let args = tree_of_typlist sch tyl in
      Otyp_class (non_gen, tree_of_path p, args)
    | _ ->
      assert false
  end

and is_non_gen sch ty =
  sch && is_Tvar ty && ty.level <> generic_level

and tree_of_typfields sch rest = function
  | [] ->
    let rest =
      match rest.desc with
        | Tvar _ | Tunivar _ -> Some (is_non_gen sch rest)
        | Tconstr _ -> Some false
        | Tnil -> None
        | _ -> assert false
    in
    ([], rest)
  | (s, t) :: l ->
    let field = (s, tree_of_typexp sch t) in
    let (fields, rest) = tree_of_typfields sch rest l in
    (field :: fields, rest)

let typexp sch prio ppf ty =
  print_out_type ppf (tree_of_typexp sch ty)

let type_scheme ppf ty = reset_and_mark_loops ty; typexp true 0 ppf ty

(* Create special buffers and formatters to allow HTML to be mixed
   into the pretty printer's output. *)
open Cow

type html_buffer = 
    { mutable stack: Cow.Html.t list;
      data: Buffer.t }
      
let html_buffer () = { stack = [Html.nil]; data = Buffer.create 80 }
  
let flush_data hb = 
  if Buffer.length hb.data <> 0 then begin
    let data = `Data (Buffer.contents hb.data) in
    Buffer.clear hb.data;
    match hb.stack with
        top :: rest -> hb.stack <- (data :: top) :: rest
      | [] -> assert false
  end

let push_level hb = 
  flush_data hb;
  hb.stack <- Html.nil :: hb.stack


let pop_level hb = 
  flush_data hb;
  match hb.stack with
      top :: rest -> 
	hb.stack <- rest;
	List.rev top
    | _ -> assert false

let add_string hb str = 
  Buffer.add_string hb.data str

let add_substring hb str ofs lens = 
  Buffer.add_substring hb.data str ofs lens

let add_html hb html = 
  match hb.stack with
      top :: rest -> 
	hb.stack <- (List.rev_append html top) :: rest
    | _ -> assert false

let formatter_of_html_buffer hb =
  make_formatter (add_substring hb) ignore
    
let with_html tagf pf a =
  let hb = html_buffer () in 
  let ppf = formatter_of_html_buffer hb in
  let mark_open_tag tag = 
    push_level hb;
    ""
  in
  let mark_close_tag tag = 
    try
      let make_html = tagf tag in
      let body = pop_level hb in
      let html = make_html body in
      add_html hb html;
      ""
    with Not_found -> ""
  in
  let tag_functions = 
    { mark_open_tag;
      mark_close_tag;
      print_open_tag = ignore;
      print_close_tag = ignore } 
  in
  pp_set_formatter_tag_functions ppf tag_functions;
  pp_set_mark_tags ppf true;
  pf ppf a;
  pp_print_flush ppf ();
  pop_level hb

(* Convert semantic tags into HTML *)
let process_tags tag =
  match tag with
    | "unresolved" -> (fun body -> <:html<$body$&>>)
    | _ -> 
      let idx = String.index tag ':' in
      let len = (String.length tag) - idx in
      let pref = String.sub tag 0 idx in 
      let arg = String.sub tag (idx + 1) (len - 1) in
      match pref with
        | "path" ->
	  (fun body -> <:html<<a href="$uri:Uri.of_string arg$">$body$</a>&>>)
	| _ -> raise Not_found
	       
let path local ?(is_class=false) p = 
  index := Some local;
  with_html process_tags (path ~is_class:is_class) p
    
let type_scheme local ty = 
  index := Some local;
  with_html process_tags type_scheme ty
