open Cow

type info = Html.t

val string_of_html : Xml.t -> string

val json_of_info: info -> Json.t

type typ = Html.t

val json_of_typ: typ -> Json.t

type path = Html.t

val json_of_path: path -> Json.t

type variant_constructor =
 { vc_name: string;
   vc_args: typ list;
   vc_info: info option }
      
val json_of_variant_constructor: variant_constructor -> Json.t

val constructor: string -> typ list ->  info option -> variant_constructor

type record_label =
 { rl_name: string;
   rl_mut: bool;
   rl_typ: typ;
   rl_info: info option }

val label: string -> bool -> typ -> info option -> record_label

type type_kind = 
  { tk_kind: [ `Abstract | `Variant | `Record ];
    tk_constructors: variant_constructor list option;
    tk_labels: record_label list option }

val json_of_type_kind: type_kind -> Json.t

val kAbstract: type_kind

val kVariant: variant_constructor list -> type_kind

val kRecord: record_label list -> type_kind

type class_type =
  { ct_kind: [ `Ident | `Sig ];
    ct_args: typ list option;
    ct_params: typ list option;
    ct_path: path option;
    ct_fields: class_type_field list option }
and class_type_field =
  { ctf_field: [ `Inherit | `Val | `Method | `Constraint | `Comment ];
    ctf_class_type: class_type option;
    ctf_name: string option;
    ctf_mut: bool option;
    ctf_virt: bool option;
    ctf_priv: bool option;
    ctf_typ: typ option;
    ctf_eq: (typ * typ) option;
    ctf_info: info option; }

val json_of_class_type: class_type -> Json.t

val json_of_class_type_field: class_type_field -> Json.t

val kClassIdent: typ list -> typ list -> path -> class_type

val kClassSig: typ list -> class_type_field list -> class_type

val fInherit: class_type -> info option -> class_type_field

val fVal: string -> bool -> bool -> bool -> typ -> 
  info option -> class_type_field 

val fMethod: string -> bool -> bool ->  typ -> info option -> class_type_field

val fConstraint: typ -> typ -> info option -> class_type_field

val fClassComment: info option -> class_type_field 

type with_constraint =
  { wc_kind: [ `Type | `Module ];
    wc_path: path;
    wc_subst: bool;
    wc_typeq: typ option;
    wc_modeq: path option }

val json_of_with_constraint: with_constraint -> Json.t

val kWithType: path -> bool -> typ -> with_constraint

val kWithMod: path -> bool -> path -> with_constraint

type variance = [ `None | `Positive | `Negative ]

val json_of_variance: variance -> Json.t

type module_type =
  { mt_kind: [ `Ident | `Sig | `Functor | `With | `TypeOf ];
    mt_path: path option;
    mt_items: signature_item list option;
    mt_arg_name: string option;
    mt_arg_type: module_type option;
    mt_cnstrs: with_constraint list option;
    mt_base: module_type option;
    mt_expr: module_expr option }
and signature_item =
  { si_item: [ `Value | `Primitive | `Type | `Exception | `Module 
          | `ModType | `Include | `Class | `ClassType | `Comment ];
    si_name: string option;
    si_typ: typ option;
    si_primitive: string list option;
    si_params: typ list option;
    si_cstrs: (typ * typ) list option;
    si_type_kind: type_kind option;
    si_priv: bool option;
    si_manifest: typ option;
    si_variance: variance list option;
    si_args: typ list option;
    si_module_type: module_type option;
    si_virt: bool option;
    si_class_type: class_type option;
    si_info: info option }
and module_expr =
  { me_kind: [ `Ident ];
    me_path: path option }

val json_of_module_type: module_type -> Json.t

val json_of_signature_item: signature_item -> Json.t

val json_of_module_expr: module_expr -> Json.t

val kModTypeIdent: path -> module_type

val kModTypeSig: signature_item list -> module_type 

val kModTypeFunctor: string -> module_type -> module_type -> module_type

val kModTypeWith: with_constraint list -> module_type -> module_type

val kModTypeTypeOf: module_expr -> module_type

val iValue: string -> typ -> info option -> signature_item

val iPrimitive: string -> typ -> string list -> info option -> signature_item

val iType: string -> typ list -> (typ * typ) list -> type_kind -> 
  bool -> typ option ->  variance list -> info option -> signature_item 

val iException: string -> typ list -> info option -> signature_item

val iModule: string -> module_type -> info option -> signature_item 

val iModType: string -> module_type option -> info option -> signature_item 
  
val iInclude: module_type -> info option -> signature_item

val iClass: string -> typ list -> variance list -> bool -> 
  class_type -> info option -> signature_item 

val iClassType: string -> typ list -> variance list -> bool -> 
  class_type -> info option -> signature_item

val iComment: info option -> signature_item

type file = 
  { f_items: signature_item list;
    f_info: info option }

val kModIdent: path -> module_expr

val json_of_file: file -> Json.t

val file: signature_item list -> info option -> file

