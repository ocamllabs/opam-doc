(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Type declarations *)

type type_kind =
    Dtype_abstract
  | Dtype_variant of (string * Info.info option) list
  | Dtype_record of (string * Info.info option) list

(* Type expressions for the class language *)

and class_type = 
    Dcty_constr
  | Dcty_signature of class_signature
  | Dcty_fun of class_type

and class_signature = class_type_field list

and class_type_field = 
  { pctf_desc: class_type_field_desc;
    pctf_info: Info.info option; }

and class_type_field_desc
    Tctf_inher of class_type
  | Tctf_val of string
  | Tctf_meth of string
  | Tctf_cstr

(* Value expressions for the class language *)

type class_expr =
    Dcl_ident
  | Dcl_structure of class_structure
  | Dcl_fun of class_expr
  | Dcl_apply of class_expr
  | Dcl_let of class_expr
  | Dcl_constraint of class_expr * class_type option

and class_structure = class_field list

and class_field =
  { dcf_desc : class_field_desc;
    dcf_info: Info.info option; }

and class_field_desc =
    Dcf_inher of class_type
  | Dcf_val of string
  | Dcf_meth of string
  | Dcf_constr
  | Dcf_init

(* Type expressions for the module language *)

and module_type =
    Dmty_ident
  | Dmty_signature of signature
  | Dmty_functor of module_type * module_type
  | Dmty_with of module_type
  | Dmty_typeof of module_expr

and signature = signature_item list

and signature_item =
  { dsig_desc: signature_item_desc;
    dsig_info: Info.info option }

and signature_item_desc =
    Dsig_value of string
  | Dsig_type of string * type_kind
  | Dsig_exception of string
  | Dsig_module of string * module_type
  | Dsig_modtype of string * module_type option
  | Dsig_open
  | Dsig_include of module_type
  | Dsig_class of class_type
  | Dsig_class_type of class_type
  | Dsig_comment
  | Dsig_stop

(* Value expressions for the module language *)

and module_expr =
    Dmod_ident
  | Dmod_structure of structure
  | Dmod_functor of module_type * module_expr
  | Dmod_apply of module_expr * module_expr
  | Dmod_constraint of module_expr * module_type
  | Dmod_unpack

and structure = structure_item list

and structure_item =
  { dstr_desc : structure_item_desc;
    dstr_info: Info.info option; }

and structure_item_desc =
    Dstr_eval
  | Dstr_value of string
  | Dstr_primitive of string
  | Dstr_type of string * type_kind
  | Dstr_exception of string
  | Dstr_module of string * module_expr
  | Dstr_modtype of string * module_type
  | Dstr_open
  | Dstr_class of string * class_expr
  | Dstr_class_type of string * class_type
  | Dstr_include of module_expr
  | Dstr_comment
  | Dstr_stop
