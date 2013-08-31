val index_file_path : unit -> string
val default_index_name : unit -> string
val filter_pervasives : unit -> bool
val clear_index : unit -> bool
val always_proceed : unit -> bool
val package_descr : unit -> string
val current_package : unit -> string
val set_current_package : string -> unit 
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
