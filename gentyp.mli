open Cow

val path: Index.local -> ?is_class:bool -> Path.t -> Html.t

val type_scheme: Index.local -> Types.type_expr -> Html.t
