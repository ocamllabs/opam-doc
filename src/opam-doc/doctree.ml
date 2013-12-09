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
  | Dtype_variant of (string * Info.t option) list
  | Dtype_record of (string * Info.t option) list

(* Type expressions for the class language *)

type class_type = 
    Dcty_constr
  | Dcty_signature of class_signature
  | Dcty_fun of class_type

and class_signature = class_type_field list

and class_type_field = 
  { dctf_desc: class_type_field_desc;
    dctf_info: Info.t option; 
    dctf_after_info: Info.t option; }

and class_type_field_desc =
    Dctf_inher of class_type
  | Dctf_val of string
  | Dctf_meth of string
  | Dctf_cstr
  | Dctf_comment
  | Dctf_stop

(* Value expressions for the class language *)

type class_expr =
    Dcl_constr
  | Dcl_structure of class_structure
  | Dcl_fun of class_expr
  | Dcl_apply of class_expr
  | Dcl_let of class_expr
  | Dcl_constraint of class_expr * class_type

and class_structure = class_field list

and class_field =
  { dcf_desc : class_field_desc;
    dcf_info: Info.t option; }

and class_field_desc =
    Dcf_inher of class_expr
  | Dcf_val of string
  | Dcf_meth of string
  | Dcf_constr
  | Dcf_init
  | Dcf_comment
  | Dcf_stop

(* Type expressions for the module language *)

type module_type =
    Dmty_ident
  | Dmty_signature of signature
  | Dmty_functor of module_type * module_type
  | Dmty_with of module_type
  | Dmty_typeof of module_expr

and signature = signature_item list

and signature_item =
  { dsig_desc: signature_item_desc;
    dsig_info: Info.t option;
    dsig_after_info: Info.t option }

and signature_item_desc =
    Dsig_value of string
  | Dsig_type of string * type_kind
  | Dsig_exception of string
  | Dsig_module of string * module_type
  | Dsig_recmodule of string * module_type
  | Dsig_modtype of string * module_type option
  | Dsig_open
  | Dsig_include of module_type
  | Dsig_class of string * class_type
  | Dsig_class_type of string * class_type
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
    dstr_info: Info.t option; }

and structure_item_desc =
    Dstr_eval
  | Dstr_value of string option
  | Dstr_primitive of string
  | Dstr_type of string * type_kind
  | Dstr_exception of string
  | Dstr_exn_rebind of string
  | Dstr_module of string * module_expr
  | Dstr_recmodule of string * module_type * module_expr
  | Dstr_modtype of string * module_type
  | Dstr_open
  | Dstr_class of string * class_expr
  | Dstr_class_type of string * class_type
  | Dstr_include of module_expr
  | Dstr_comment
  | Dstr_stop

type interface = 
  { dintf_items: signature_item list;
    dintf_info: Info.t option; }

type implementation = 
  { dimpl_items: structure_item list;
    dimpl_info: Info.t option; }

type file = 
  Dfile_intf of interface
| Dfile_impl of implementation
