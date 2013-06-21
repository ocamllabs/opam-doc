open Cow

type info = Html.t

let rec output_html_data out = function
  | (`Data _ as d) :: t ->
    Xml.output out d;
    output_html_data out t
  | (`El _ as e) :: t ->
    Xml.output_tree (fun x -> x) out e;
    Xml.output out (`Dtd None);
    output_html_data out t
  | [] -> ()

let string_of_html html = 
  let b = Buffer.create 256 in
  let out = Xml.make_output (`Buffer b) in
    Xml.output out (`Dtd None);
    output_html_data out html;
    Buffer.contents b

let json_of_info i = 
  Json.String (string_of_html i)

let list_opt = function
  | [] -> None
  | l -> Some l

let info_of_json _ = raise (Failure "Not Supported")

type typ = Html.t

let json_of_typ ty = Json.String (string_of_html ty)

let typ_of_json _ = raise (Failure "Not Supported")

type path = Html.t

let json_of_path p = Json.String (string_of_html p)

let path_of_json _ = raise (Failure "Not Supported")

type variant_constructor =
 { vc_name: string;
   vc_args: typ list;
   vc_info: info option }
with json

let constructor vc_name vc_args vc_info = {vc_name; vc_args; vc_info}

type record_label =
 { rl_name: string;
   rl_mut: bool;
   rl_typ: typ;
   rl_info: info option }
with json

let label rl_name rl_mut rl_typ rl_info = {rl_name; rl_mut; rl_typ; rl_info}

type type_kind = 
  { tk_kind: [ `Abstract | `Variant | `Record ];
    tk_constructors: variant_constructor list option;
    tk_labels: record_label list option }
with json

let kAbstract = 
  { tk_kind = `Abstract;
    tk_constructors = None;
    tk_labels = None }

let kVariant constrs = 
  { tk_kind = `Variant; 
    tk_constructors = Some constrs; 
    tk_labels = None }

let kRecord labels = 
  { tk_kind = `Record;
    tk_constructors = None;
    tk_labels = Some labels }

type class_type =
  { ct_kind: [ `Ident | `Sig | `Constraint ];
    ct_args: typ list option;
    ct_params: typ list option;
    ct_path: path option;
    ct_fields: class_type_field list option;
    ct_cstr : (class_type * class_type) option;
  }

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
with json

let kClassIdent args params path = 
  { ct_kind = `Ident; 
    ct_args = list_opt args;
    ct_params = Some params;
    ct_path = Some path;
    ct_fields = None;
    ct_cstr = None; }

let kClassSig args fields =
  { ct_kind = `Sig;
    ct_args = list_opt args;
    ct_params = None;
    ct_path = None;
    ct_fields = Some fields;
    ct_cstr = None;  }

(* params or not params*)
let kClassConstraint args constr = 
  { ct_kind = `Constraint;
    ct_args = list_opt args;
    ct_params = None;
    ct_path = None;
    ct_fields = None;
    ct_cstr = Some constr; 
  }

let fInherit class_type info = 
  { ctf_field = `Inherit;
    ctf_class_type = Some class_type;
    ctf_name = None;
    ctf_mut = None;
    ctf_virt = None;
    ctf_priv = None;
    ctf_typ = None;
    ctf_eq = None;
    ctf_info = info }

let fVal name mut virt (*priv*) typ info = 
  { ctf_field = `Val;
    ctf_class_type = None;
    ctf_name = Some name;
    ctf_mut = Some mut;
    ctf_virt = Some virt;
    ctf_priv = None(* Some priv*);
    ctf_typ = Some typ;
    ctf_eq = None;
    ctf_info = info }

let fMethod name virt priv typ info = 
  { ctf_field = `Method;
    ctf_class_type = None;
    ctf_name = Some name;
    ctf_mut = None;
    ctf_virt = Some virt;
    ctf_priv = Some priv;
    ctf_typ = Some typ;
    ctf_eq = None;
    ctf_info = info }

let fConstraint typ1 typ2 info = 
  { ctf_field = `Constraint;
    ctf_class_type = None;
    ctf_name = None;
    ctf_mut = None;
    ctf_virt = None;
    ctf_priv = None;
    ctf_typ = None;
    ctf_eq = Some (typ1, typ2);
    ctf_info = info }

let fClassComment info = 
  { ctf_field = `Comment;
    ctf_class_type = None;
    ctf_name = None;
    ctf_mut = None;
    ctf_virt = None;
    ctf_priv = None;
    ctf_typ = None;
    ctf_eq = None;
    ctf_info = info }

type with_constraint =
  { wc_kind: [ `Type | `Module ];
    wc_path: path;
    wc_subst: bool;
    wc_typeq: typ option;
    wc_modeq: path option }
with json

let kWithType path subst typeq = 
  { wc_kind = `Type;
    wc_path = path;
    wc_subst = subst;
    wc_typeq = Some typeq;
    wc_modeq = None }

let kWithMod path subst modeq = 
  { wc_kind = `Type;
    wc_path = path;
    wc_subst = subst;
    wc_typeq = None;
    wc_modeq = Some modeq }

type variance = [ `None | `Positive | `Negative ]
with json

type module_type =
  { mt_kind: [ `Ident | `Sig | `Functor | `With | `TypeOf | `Apply ];
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
with json

let kModTypeIdent path = 
  { mt_kind = `Ident;
    mt_path = Some path;
    mt_items = None;
    mt_arg_name = None;
    mt_arg_type = None;
    mt_cnstrs = None;
    mt_base = None;
    mt_expr = None }

let kModTypeSig items = 
  { mt_kind = `Sig;
    mt_path = None;
    mt_items = Some items;
    mt_arg_name = None;
    mt_arg_type = None;
    mt_cnstrs = None;
    mt_base = None;
    mt_expr = None }

let kModTypeFunctor arg_name arg_type base = 
  { mt_kind = `Functor;
    mt_path = None;
    mt_items = None;
    mt_arg_name = Some arg_name;
    mt_arg_type = Some arg_type;
    mt_cnstrs = None;
    mt_base = Some base;
    mt_expr = None }

let kModTypeWith cnstrs base = 
  { mt_kind = `With;
    mt_path = None;
    mt_items = None;
    mt_arg_name = None;
    mt_arg_type = None;
    mt_cnstrs = Some cnstrs;
    mt_base = Some base;
    mt_expr = None }

let kModTypeTypeOf expr = 
  { mt_kind = `TypeOf;
    mt_path = None;
    mt_items = None;
    mt_arg_name = None;
    mt_arg_type = None;
    mt_cnstrs = None;
    mt_base = None;
    mt_expr = Some expr }

let kModTypeApply base arg_type = 
  { mt_kind = `Apply;
    mt_path = None;
    mt_items = None;
    mt_arg_name = None;
    mt_arg_type = Some arg_type;
    mt_cnstrs = None;
    mt_base = Some base;
    mt_expr = None; }

let iValue name typ info = 
  { si_item = `Value;
    si_name = Some name;
    si_typ = Some typ;
    si_primitive = None;
    si_params = None;
    si_cstrs = None;
    si_type_kind = None;
    si_priv = None;
    si_manifest = None;
    si_variance = None;
    si_args = None;
    si_module_type = None;
    si_virt = None;
    si_class_type = None;
    si_info = info }

let iPrimitive name typ primitive info = 
  { si_item = `Primitive;
    si_name = Some name;
    si_typ = Some typ;
    si_primitive = Some primitive;
    si_params = None;
    si_cstrs = None;
    si_type_kind = None;
    si_priv = None;
    si_manifest = None;
    si_variance = None;
    si_args = None;
    si_module_type = None;
    si_virt = None;
    si_class_type = None;
    si_info = info }

let iType name params cstrs type_kind priv manifest variance info = 
  { si_item = `Type;
    si_name = Some name;
    si_typ = None;
    si_primitive = None;
    si_params = Some params;
    si_cstrs = list_opt cstrs;
    si_type_kind = Some type_kind;
    si_priv = Some priv;
    si_manifest = manifest;
    si_variance = Some variance;
    si_args = None;
    si_module_type = None;
    si_virt = None;
    si_class_type = None;
    si_info = info }

let iException name args info = 
  { si_item = `Exception;
    si_name = Some name;
    si_typ = None;
    si_primitive = None;
    si_params = None;
    si_cstrs = None;
    si_type_kind = None;
    si_priv = None;
    si_manifest = None;
    si_variance = None;
    si_args = Some args;
    si_module_type = None;
    si_virt = None;
    si_class_type = None;
    si_info = info }

let iModule name module_type info = 
  { si_item = `Module;
    si_name = Some name;
    si_typ = None;
    si_primitive = None;
    si_params = None;
    si_cstrs = None;
    si_type_kind = None;
    si_priv = None;
    si_manifest = None;
    si_variance = None;
    si_args = None;
    si_module_type = Some module_type;
    si_virt = None;
    si_class_type = None;
    si_info = info }

let iModType name module_type info = 
  { si_item = `ModType;
    si_name = Some name;
    si_typ = None;
    si_primitive = None;
    si_params = None;
    si_cstrs = None;
    si_type_kind = None;
    si_priv = None;
    si_manifest = None;
    si_variance = None;
    si_args = None;
    si_module_type = module_type;
    si_virt = None;
    si_class_type = None;
    si_info = info }

let iInclude module_type info = 
  { si_item = `Include;
    si_name = None;
    si_typ = None;
    si_primitive = None;
    si_params = None;
    si_cstrs = None;
    si_type_kind = None;
    si_priv = None;
    si_manifest = None;
    si_variance = None;
    si_args = None;
    si_module_type = Some module_type;
    si_virt = None;
    si_class_type = None;
    si_info = info }

let iClass name params variance virt class_type info = 
  { si_item = `Class;
    si_name = Some name;
    si_typ = None;
    si_primitive = None;
    si_params = Some params;
    si_cstrs = None;
    si_type_kind = None;
    si_priv = None;
    si_manifest = None;
    si_variance = Some variance;
    si_args = None;
    si_module_type = None;
    si_virt = Some virt;
    si_class_type = Some class_type;
    si_info = info }

let iClassType name params variance virt class_type info = 
  { si_item = `ClassType;
    si_name = Some name;
    si_typ = None;
    si_primitive = None;
    si_params = Some params;
    si_cstrs = None;
    si_type_kind = None;
    si_priv = None;
    si_manifest = None;
    si_variance = Some variance;
    si_args = None;
    si_module_type = None;
    si_virt = Some virt;
    si_class_type = Some class_type;
    si_info = info }

let iComment info = 
  { si_item = `Comment;
    si_name = None;
    si_typ = None;
    si_primitive = None;
    si_params = None;
    si_cstrs = None;
    si_type_kind = None;
    si_priv = None;
    si_manifest = None;
    si_variance = None;
    si_args = None;
    si_module_type = None;
    si_virt = None;
    si_class_type = None;
    si_info = info }

let kModIdent path = 
  { me_kind = `Ident;
    me_path = Some path }

type file = 
  { f_items: signature_item list;
    f_info: info option }
with json

let file f_items f_info = {f_items; f_info}

