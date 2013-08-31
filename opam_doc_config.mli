val index_file_path : string ref
val default_index_name : string ref
val filter_pervasives : bool ref
val clear_index : bool ref
val always_proceed : bool ref
val package_descr : string ref
val current_package : string ref
val options : (string * Arg.spec * string) list
val usage : string
val doctype : string
val character_encoding : Cow.Xml.t
val default_stylesheet : string
type mark =
    Attribute
  | Type
  | Type_elt
  | Function
  | Exception
  | Value
  | Method
  | Title
val style_filename : string
val style_tag : Cow.Xml.t
val script_filename : string
val script_tag : Cow.Xml.t
val default_script : string
