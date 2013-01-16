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

type style_kind =
  | SK_bold
  | SK_italic
  | SK_emphasize
  | SK_center
  | SK_left
  | SK_right
  | SK_superscript
  | SK_subscript
  | SK_custom of string

type ref_kind =
    RK_element
  | RK_module
  | RK_module_type
  | RK_class
  | RK_class_type
  | RK_value
  | RK_type
  | RK_exception
  | RK_attribute
  | RK_method
  | RK_section
  | RK_recfield
  | RK_const
  | RK_link
  | RK_custom of string

type special_ref_kind =
    SRK_module_list of string list
  | SRK_index_list

and text_element =
  | Raw of string
  | Code of string
  | CodePre of string
  | Verbatim of string
  | Style of style_kind * text
  | List of text list
  | Enum of text list
  | Newline
  | Block of text
  | Title of int * string option * text
  | Ref of string * ref_kind * text option
  | Special_ref of special_ref_kind
  | Target of string option * string

and text = text_element list

type see_ref =
    See_url of string
  | See_file of string
  | See_doc of string

type see = see_ref * text

type param = (string * text)

type raised_exception = (string * text)

type t = {
    i_desc : text option;
    i_authors : string list;
    i_version : string option;
    i_sees : see list;
    i_since : string option;
    i_before : (string * text) list;
    i_deprecated : text option;
    i_params : param list;
    i_raised_exceptions : raised_exception list;
    i_return_value : text option ;
    i_custom : (string * text) list ;
  }

let dummy = {
  i_desc = None ;
  i_authors = [] ;
  i_version = None ;
  i_sees = [] ;
  i_since = None ;
  i_before = [] ;
  i_deprecated = None ;
  i_params = [] ;
  i_raised_exceptions = [] ;
  i_return_value = None ;
  i_custom = [] ;
}
