val generate_file_from_interface :
  Index.local ->
  string -> Doctree.file option -> Typedtree.signature -> string * Cow.Html.t

val generate_file_from_structure :
  Index.local ->
  string -> Doctree.file option -> Typedtree.structure -> string * Cow.Html.t
