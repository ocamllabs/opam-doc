(** Basic operations on HTML *)

(** Html concatenation *)
val concat : Cow.Html.t list -> Cow.Html.t

(** Html concatenation with a seperator *)
val insert_between : string -> Cow.Html.t list -> Cow.Html.t

(** Conversion from string to html *)
val html_of_string : string -> Cow.Html.t

(** Conversion from html to string *)
val string_of_html : Cow.Html.t -> string

(** Wrap the content into a \<code\> html tag with a class label option *)
val code : ?cls:string -> Cow.Html.t -> Cow.Html.t

(** Wrap the content into a <pre> html tag*)
val pre : Cow.Html.t -> Cow.Html.t
