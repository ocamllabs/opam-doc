open Format
open Doctree

val module_type : int -> formatter -> module_type -> unit
val signature :
  int -> formatter -> signature_item list -> unit
val signature_item :
  int -> formatter -> signature_item -> unit
val module_expr : int -> formatter -> module_expr -> unit
val structure : int -> formatter -> structure -> unit
val structure_item : int -> formatter -> structure_item -> unit
val interface : int -> formatter -> interface -> unit
val implementation : int -> formatter -> implementation -> unit
val file : int -> formatter -> file -> unit
