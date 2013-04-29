open Format
open Doctree
open Info

let line i f s (*...*) =
  fprintf f "%s" (String.make (2*i) ' ');
  fprintf f s (*...*)


let list i f ppf l =
  match l with
  | [] -> line i ppf "[]\n"
  | _ :: _ ->
     line i ppf "[\n";
     List.iter (f (i+1) ppf) l;
     line i ppf "]\n"

let option i f ppf x =
  match x with
  | None -> line i ppf "None\n";
  | Some x ->
      line i ppf "Some\n";
      f (i+1) ppf x

let string i ppf s = line i ppf "\"%s\"\n" s

let text_element i ppf x = 
  line i ppf "text_element\n";
  let i = (i+1) in
  match x with
  | Raw s -> line i ppf "Raw \"%s\"\n" s
  | Code s -> line i ppf "Code \"%s\"\n" s
  | PreCode s -> line i ppf "PreCode \"%s\"\n" s
  | Verbatim s -> line i ppf "Verbatim \"%s\"\n" s
  | Style _ -> line i ppf "STYLE\n"
  | List _ -> line i ppf "LISR\n"
  | Enum _ -> line i ppf "ENUM\n"
  | Newline -> line i ppf "Newline\n"
  | Block _ -> line i ppf "BLOCK\n"
  | Title _ -> line i ppf "TITLE\n"
  | Ref _ -> line i ppf "REF\n"
  | Special_ref _ -> line i ppf "SPECIAL_REF\n"
  | Target _ -> line i ppf "TARGET\n"

let text i ppf x = list i text_element ppf x

let see i ppf x = line i ppf "SEE\n"

let info i ppf x = 
  line i ppf "info\n";
  let i = (i+1) in
  line i ppf "i_desc =\n";
  option (i+1) text ppf x.i_desc;
  line i ppf "i_authors =\n";
  list (i+1) string ppf x.i_authors;
  line i ppf "i_version =\n";
  option (i+1) string ppf x.i_version;
  line i ppf "i_sees =\n";
  list (i+1) see ppf x.i_sees;
  line i ppf "i_since =\n";
  option (i+1) string ppf x.i_since;
  line i ppf "i_before =\n";
  list (i+1) (fun i ppf _ -> line i ppf "BEFORE\n") ppf x.i_before;
  line i ppf "i_deprecated =\n";
  option (i+1) text ppf x.i_deprecated;
  line i ppf "i_params =\n";
  list (i+1) (fun i ppf _ -> line i ppf "PARAM\n") ppf x.i_params;
  line i ppf "i_raised_exceptions =\n";
  list (i+1) (fun i ppf _ -> line i ppf "RAISED_EXCEPTION\n") ppf x.i_raised_exceptions;
  line i ppf "i_return_value =\n";
  option (i+1) text ppf x.i_return_value;
  line i ppf "i_custom =\n";
  list (i+1) (fun i ppf _ -> line i ppf "CUSTOM\n") ppf x.i_custom

let string_x_info_option i ppf (s, i_opt) =
  line i ppf "\"%s\"\n" s;
  option (i+1) info ppf i_opt

let type_kind i ppf x =
  match x with
    Dtype_abstract -> 
      line i ppf "Dtype_abstract\n"
  | Dtype_variant l -> 
      line i ppf "Dtype_variant\n";
      list (i+1) string_x_info_option ppf l;
  | Dtype_record l ->
      line i ppf "Dtype_record\n";
      list (i+1) string_x_info_option ppf l

let rec class_type i ppf x =
  match x with
    Dcty_constr -> 
      line i ppf "Dcty_constr\n"
  | Dcty_signature cs -> 
      line i ppf "Dcty_signature\n";
      class_signature (i+1) ppf cs
  | Dcty_fun ct ->
      line i ppf "Dcty_fun\n";
      class_type (i+1) ppf ct

and class_signature i ppf x = list i class_type_field ppf x 

and class_type_field i ppf x = 
  line i ppf "CLASS_TYPE_FIELD\n"

and class_expr i ppf x = 
  line i ppf "CLASS_EXPR\n"

and class_structure i ppf x = list i class_field ppf x

and class_field i ppf x = 
  line i ppf "CLASS_FIELD\n"

(* Type expressions for the module language *)

and module_type i ppf x = 
  line i ppf "MODULE_TYPE\n"

and signature i ppf x = list i signature_item ppf x

and signature_item i ppf x = 
  line i ppf "signature_item\n";
  let i = i+1 in
  line i ppf "info\n";
  option (i+1) info ppf x.dsig_info;
  line i ppf "after_info\n";
  option (i+1) info ppf x.dsig_after_info;
  match x.dsig_desc with
  | Dsig_value s -> line i ppf "Dsig_value \"%s\"\n" s
  | Dsig_type(s, tk) ->
      line i ppf "Dsig_type \"%s\"\n" s;
      type_kind i ppf tk
  | Dsig_exception s -> line i ppf "Dsig_exception \"%s\"\n" s
  | Dsig_module(s, mt) ->
      line i ppf "Dsig_module \"%s\"\n" s;
      module_type i ppf mt
  | Dsig_recmodule(s, mt) ->
      line i ppf "Dsig_recmodule \"%s\"\n" s;
      module_type i ppf mt
  | Dsig_modtype(s, mt_opt) ->
      line i ppf "Dsig_modtype \"%s\"\n" s;
      option i module_type ppf mt_opt
  | Dsig_open -> line i ppf "Dsig_open\n"
  | Dsig_include mt ->
      line i ppf "Dsig_include\n";
      module_type i ppf mt
  | Dsig_class(s, ct) ->
      line i ppf "Dsig_class \"%s\"\n" s;
      class_type i ppf ct
  | Dsig_class_type(s, ct) ->
      line i ppf "Dsig_class_type \"%s\"\n" s;
      class_type i ppf ct
  | Dsig_comment -> line i ppf "Dsig_comment\n"
  | Dsig_stop -> line i ppf "Dsig_stop\n"

and module_expr i ppf x = 
  line i ppf "MODULE_EXPR\n"

and structure i ppf x = list i structure_item ppf x

and structure_item i ppf x = 
  line i ppf "STRUCTURE_ITEM\n"

and interface i ppf x = 
  line i ppf "INTERFACE\n"

and implementation i ppf x = 
  line i ppf "IMPLEMENTATION\n"

and file i ppf x = 
  line i ppf "FILE\n"
