open Cow

type info = Html.t

val json_of_info: info -> Json.t

type typ = Html.t

val json_of_typ: typ -> Json.t

type path = Html.t

val json_of_path: path -> Json.t

type variant_constructor

val json_of_variant_constructor: variant_constructor -> Json.t

val constructor: string -> typ list ->  info option -> variant_constructor

type record_label

val json_of_record_label: record_label -> Json.t

val label: string -> bool -> typ -> info option -> record_label

type type_kind

val json_of_type_kind: type_kind -> Json.t

val kAbstract: type_kind

val kVariant: variant_constructor list -> type_kind

val kRecord: record_label list -> type_kind

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

type variance

val json_of_variance: variance -> Json.t

val vNone: variance

val vPositive: variance

val vNegative: variance

type module_type

val json_of_module_type: module_type -> Json.t

type signature_item

val json_of_signature_item: signature_item -> Json.t

type module_expr

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

val kModIdent: path -> module_expr

type file

val json_of_file: file -> Json.t

val file: signature_item list -> info option -> file
