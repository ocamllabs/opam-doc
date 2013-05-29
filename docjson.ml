open Cow

type info = Html.t

let id x = x

let rec output_html_data out = function
  | (`Data _ as d) :: t ->
    Xml.output out d;
    output_html_data out t
  | (`El _ as e) :: t ->
    Xml.output_tree id out e;
    Xml.output out (`Dtd None);
    output_html_data out t
  | [] -> ()

let string_of_html html = 
  let b = Buffer.create 32 in
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
 { name: string;
   args: typ list;
   info: info option }
with json

let constructor name args info = {name; args; info}

let triplet_of_constructor  = 
  function {name=n; args=ags; info=info} -> n, ags, info

module Record_label = struct
type record_label =
 { name: string;
   mut: bool;
   typ: typ;
   info: info option }
with json
end
open Record_label



let record_fields = 
  function  { name=n; mut=b; typ=t; info=i} -> n,b,t,i

let label name mut typ info = {name; mut; typ; info}

type type_kind = 
  { kind: [ `Abstract | `Variant | `Record ];
    constructors: variant_constructor list option;
    labels: record_label list option }
with json

let kAbstract = 
  { kind = `Abstract;
    constructors = None;
    labels = None }

let kVariant constrs = 
  { kind = `Variant; 
    constructors = Some constrs; 
    labels = None }

let kRecord labels = 
  { kind = `Record;
    constructors = None;
    labels = Some labels }

type class_type =
  { kind: [ `Ident | `Sig ];
    args: typ list option;
    params: typ list option;
    path: path option;
    fields: class_type_field list option }

and class_type_field =
  { field: [ `Inherit | `Val | `Method | `Constraint | `Comment ];
    class_type: class_type option;
    name: string option;
    mut: bool option;
    virt: bool option;
    priv: bool option;
    typ: typ option;
    eq: (typ * typ) option;
    info: info option; }
with json

let kClassIdent args params path = 
  { kind = `Ident; 
    args = list_opt args;
    params = Some params;
    path = Some path;
    fields = None }

let kClassSig args fields =
  { kind = `Sig;
    args = list_opt args;
    params = None;
    path = None;
    fields = Some fields }

let fInherit class_type info = 
  { field = `Inherit;
    class_type = Some class_type;
    name = None;
    mut = None;
    virt = None;
    priv = None;
    typ = None;
    eq = None;
    info = info }

let fVal name mut virt priv typ info = 
  { field = `Val;
    class_type = None;
    name = Some name;
    mut = Some mut;
    virt = Some virt;
    priv = Some priv;
    typ = Some typ;
    eq = None;
    info = info }

let fMethod name virt priv typ info = 
  { field = `Method;
    class_type = None;
    name = Some name;
    mut = None;
    virt = Some virt;
    priv = Some priv;
    typ = Some typ;
    eq = None;
    info = info }

let fConstraint typ1 typ2 info = 
  { field = `Constraint;
    class_type = None;
    name = None;
    mut = None;
    virt = None;
    priv = None;
    typ = None;
    eq = Some (typ1, typ2);
    info = info }

let fClassComment info = 
  { field = `Comment;
    class_type = None;
    name = None;
    mut = None;
    virt = None;
    priv = None;
    typ = None;
    eq = None;
    info = info }

type with_constraint =
  { kind: [ `Type | `Module ];
    path: path;
    subst: bool;
    typeq: typ option;
    modeq: path option }
with json

let kWithType path subst typeq = 
  { kind = `Type;
    path = path;
    subst = subst;
    typeq = Some typeq;
    modeq = None }

let kWithMod path subst modeq = 
  { kind = `Type;
    path = path;
    subst = subst;
    typeq = None;
    modeq = Some modeq }

type variance = [ `None | `Positive | `Negative ]
with json

let vNone = `None

let vPositive = `Positive

let vNegative = `Negative

type module_type =
  { mtkind: [ `Ident | `Sig | `Functor | `With | `TypeOf ];
    mtpath: path option;
    items: signature_item list option;
    arg_name: string option;
    arg_type: module_type option;
    cnstrs: with_constraint list option;
    base: module_type option;
    expr: module_expr option }

and signature_item =
  { item: [ `Value | `Primitive | `Type | `Exception | `Module 
          | `ModType | `Include | `Class | `ClassType | `Comment ];
    name: string option;
    typ: typ option;
    primitive: string list option;
    params: typ list option;
    cstrs: (typ * typ) list option;
    type_kind: type_kind option;
    priv: bool option;
    manifest: typ option;
    variance: variance list option;
    args: typ list option;
    module_type: module_type option;
    virt: bool option;
    class_type: class_type option;
    info: info option }

and module_expr =
  { mkind: [ `Ident ];
    mpath: path option }
with json

let kModTypeIdent path = 
  { mtkind = `Ident;
    mtpath = Some path;
    items = None;
    arg_name = None;
    arg_type = None;
    cnstrs = None;
    base = None;
    expr = None }

let kModTypeSig items = 
  { mtkind = `Sig;
    mtpath = None;
    items = Some items;
    arg_name = None;
    arg_type = None;
    cnstrs = None;
    base = None;
    expr = None }

let kModTypeFunctor arg_name arg_type base = 
  { mtkind = `Functor;
    mtpath = None;
    items = None;
    arg_name = Some arg_name;
    arg_type = Some arg_type;
    cnstrs = None;
    base = Some base;
    expr = None }

let kModTypeWith cnstrs base = 
  { mtkind = `With;
    mtpath = None;
    items = None;
    arg_name = None;
    arg_type = None;
    cnstrs = Some cnstrs;
    base = Some base;
    expr = None }

let kModTypeTypeOf expr = 
  { mtkind = `TypeOf;
    mtpath = None;
    items = None;
    arg_name = None;
    arg_type = None;
    cnstrs = None;
    base = None;
    expr = Some expr }

let iValue name typ info = 
  { item = `Value;
    name = Some name;
    typ = Some typ;
    primitive = None;
    params = None;
    cstrs = None;
    type_kind = None;
    priv = None;
    manifest = None;
    variance = None;
    args = None;
    module_type = None;
    virt = None;
    class_type = None;
    info = info }

(* bug : cannot infere type ??*)
let get_info_sig_item item = item.info

let iPrimitive name typ primitive info = 
  { item = `Primitive;
    name = Some name;
    typ = Some typ;
    primitive = Some primitive;
    params = None;
    cstrs = None;
    type_kind = None;
    priv = None;
    manifest = None;
    variance = None;
    args = None;
    module_type = None;
    virt = None;
    class_type = None;
    info = info }

let iType name params cstrs type_kind priv manifest variance info = 
  { item = `Type;
    name = Some name;
    typ = None;
    primitive = None;
    params = Some params;
    cstrs = list_opt cstrs;
    type_kind = Some type_kind;
    priv = Some priv;
    manifest = manifest;
    variance = Some variance;
    args = None;
    module_type = None;
    virt = None;
    class_type = None;
    info = info }

let iException name args info = 
  { item = `Exception;
    name = Some name;
    typ = None;
    primitive = None;
    params = None;
    cstrs = None;
    type_kind = None;
    priv = None;
    manifest = None;
    variance = None;
    args = Some args;
    module_type = None;
    virt = None;
    class_type = None;
    info = info }

let iModule name module_type info = 
  { item = `Module;
    name = Some name;
    typ = None;
    primitive = None;
    params = None;
    cstrs = None;
    type_kind = None;
    priv = None;
    manifest = None;
    variance = None;
    args = None;
    module_type = Some module_type;
    virt = None;
    class_type = None;
    info = info }

let iModType name module_type info = 
  { item = `ModType;
    name = Some name;
    typ = None;
    primitive = None;
    params = None;
    cstrs = None;
    type_kind = None;
    priv = None;
    manifest = None;
    variance = None;
    args = None;
    module_type = module_type;
    virt = None;
    class_type = None;
    info = info }

let iInclude module_type info = 
  { item = `Include;
    name = None;
    typ = None;
    primitive = None;
    params = None;
    cstrs = None;
    type_kind = None;
    priv = None;
    manifest = None;
    variance = None;
    args = None;
    module_type = Some module_type;
    virt = None;
    class_type = None;
    info = info }

let iClass name params variance virt class_type info = 
  { item = `Class;
    name = Some name;
    typ = None;
    primitive = None;
    params = Some params;
    cstrs = None;
    type_kind = None;
    priv = None;
    manifest = None;
    variance = Some variance;
    args = None;
    module_type = None;
    virt = Some virt;
    class_type = Some class_type;
    info = info }

let iClassType name params variance virt class_type info = 
  { item = `ClassType;
    name = Some name;
    typ = None;
    primitive = None;
    params = Some params;
    cstrs = None;
    type_kind = None;
    priv = None;
    manifest = None;
    variance = Some variance;
    args = None;
    module_type = None;
    virt = Some virt;
    class_type = Some class_type;
    info = info }

let iComment info = 
  { item = `Comment;
    name = None;
    typ = None;
    primitive = None;
    params = None;
    cstrs = None;
    type_kind = None;
    priv = None;
    manifest = None;
    variance = None;
    args = None;
    module_type = None;
    virt = None;
    class_type = None;
    info = info }

let kModIdent path = 
  { mkind = `Ident;
    mpath = Some path }

type file = 
  { items: signature_item list;
    info: info option }
with json

let file items info = {items; info}
