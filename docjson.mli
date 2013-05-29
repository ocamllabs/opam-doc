open Cow

type info = Html.t

val string_of_html : Xml.t -> string

val json_of_info: info -> Json.t

type typ = Html.t

val json_of_typ: typ -> Json.t

type path = Html.t

val json_of_path: path -> Json.t

type variant_constructor = 
    { name: string;
      args: typ list;
      info: info option }
      
val json_of_variant_constructor: variant_constructor -> Json.t

val constructor: string -> typ list ->  info option -> variant_constructor

(* + *)
val triplet_of_constructor : variant_constructor -> string * typ list * info option

module Record_label : sig
  type record_label =  
      { name: string;
	mut: bool;
	typ: typ;
	info: info option }
end

(* + *)
val record_fields : Record_label.record_label -> string * bool * typ * info option

(* val json_of_record_label: Record_label.record_label -> Json.t *)

val label: string -> bool -> typ -> info option -> Record_label.record_label

type type_kind =  
    { kind: [ `Abstract | `Variant | `Record ];
      constructors: variant_constructor list option;
      labels: Record_label.record_label list option }

val json_of_type_kind: type_kind -> Json.t

val kAbstract: type_kind

val kVariant: variant_constructor list -> type_kind

val kRecord: Record_label.record_label list -> type_kind

type class_type

val json_of_class_type: class_type -> Json.t

type class_type_field

val json_of_class_type_field: class_type_field -> Json.t

val kClassIdent: typ list -> typ list -> path -> class_type

val kClassSig: typ list -> class_type_field list -> class_type

val fInherit: class_type -> info option -> class_type_field

val fVal: string -> bool -> bool -> bool -> typ -> 
  info option -> class_type_field 

val fMethod: string -> bool -> bool ->  typ -> info option -> class_type_field

val fConstraint: typ -> typ -> info option -> class_type_field

val fClassComment: info option -> class_type_field 

type with_constraint

val json_of_with_constraint: with_constraint -> Json.t

val kWithType: path -> bool -> typ -> with_constraint

val kWithMod: path -> bool -> path -> with_constraint

type variance = [ `None | `Positive | `Negative ]

val json_of_variance: variance -> Json.t

val vNone: variance

val vPositive: variance

val vNegative: variance

type module_type

val json_of_module_type: module_type -> Json.t

type signature_item =
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

val json_of_signature_item: signature_item -> Json.t

val kModIdent: path -> module_type

val kModSig: signature_item list -> module_type 

val kFunctor: string -> module_type -> module_type -> module_type

val kWith: with_constraint list -> module_type -> module_type

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
    { items: signature_item list;
      info: info option }

val json_of_file: file -> Json.t

val file: signature_item list -> info option -> file

val get_info_sig_item : signature_item -> info option
