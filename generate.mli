(** Generations from cmt to html *)

(** Generate the html page for the provided cmti file *)
val generate_file_from_interface :
  Index.local ->
  string -> Doctree.file option -> Typedtree.signature -> string * Cow.Html.t

(** Generate the html page for the provided cmt file *)
val generate_file_from_structure :
  Index.local ->
  string -> Doctree.file option -> Typedtree.structure -> string * Cow.Html.t
