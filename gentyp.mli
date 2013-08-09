(** Types pretty-printing functions and references solver *)

open Cow

type path = Unresolved of string | Resolved of Uri.t * string | Apply of path * path

val html_of_path : path -> Html.t

val path: Index.local -> bool -> Path.t -> path

val type_scheme: Index.local -> Types.type_expr -> Html.t
