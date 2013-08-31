(** Utility functions to generate html tags and html pages (+ quick hacks to be removed)  *)

open Cow

(** {3 Utility functions} *)

(** Html folding with a seperator *)
val insert_between : sep:string -> Html.t list -> Html.t

(** Conversion from string to html *)
val html_of_string : string -> Html.t

(** Conversion from html to string *)
val string_of_html : Html.t -> string

(** {3 Html tags generators} *)

(** Generate a <div class="info">xxx</div> tag or Html.nil if None*)
val make_info : Html.t option -> Html.t

(** Wrap the content into a <pre> html tag*)
val make_pre : Html.t -> Html.t

(** Wrap the content into a <span> html tag with a class label option *)
val make_span : ?css_class:string -> Html.t -> Html.t

(** Wrap the content into a <span class="keyword"> html tag *)
val keyword : string -> Html.t

(** Wrap the content into a <span class="constructor"> html tag *)
val constructor : string -> Html.t

(** Wrap the content into a \<code\> html tag with a class label option *)
val code : string -> Html.t -> Html.t

(** Wrap the column list into a <table class="typetable"> by wraping the elements with <tr> tags. The elements should already have the appropriates <td> tags *)
val make_type_table :
  Html.t list -> Html.t

(** Wrap the content into a <span> that will contain a class attribute with the unique id used by the javascript to lookup types, classes, values, ...
    The html inlining prevents using id because two tags with the attribute : id="TYPEt" could also be defined in submodules and therefore false the search. 
    [Html_utils.generate_mark mark element_name content] 
*)
val generate_mark : Opam_doc_config.mark -> string -> Html.t -> Html.t

(** Generate the <td> for a class field comment *)
val make_field_comment : Html.t -> Html.t

(** Generate the <td> for a variant element *)
val make_variant_cell : 
  string -> string -> Html.t list -> Html.t option -> Html.t

(** Generate the <td> for a record element *)
val make_record_label_cell : 
  string -> string -> bool -> Html.t -> Html.t option -> Html.t

(** Generate the type parameters for a type signature.
    e.g: (+'a,-'b) for the type (+'a,-'b) t *)
val html_of_type_param_list : 
  Html.t list -> [< `Negative | `None | `Positive ] list -> Html.t

(** Generate the class parameters for a class signature.
    e.g: (+'a,-'b) for the type (+'a,-'b) t *)
val html_of_type_class_param_list :
  Html.t list -> [< `Negative | `None | `Positive ] list -> Html.t

(** Parse the include's module_type to retrieve the contained elements and return it as a json array *)
val js_array_of_include_items : Types.module_type -> string

(** {3 Html page generation} *)

(** Writes the style file if non-existent *)
val output_style_file : unit -> unit

(** Writes the script file if non-existent *)
val output_script_file : unit -> unit

(** Generate the current package module index. Elements should be of the form : 
    name * description  *)
val generate_package_index : (string * Html.t) list -> unit

(** Generate the main page that displays every package contained in the global index file *)
val generate_global_packages_index : Index.global -> unit

(** Wrap the module signature elements into a special <div> used by the javascript lookup *) 
val create_module_signature_content : Html.t list -> Html.t

(** Wrap the class signature elements into a special <div> used by the javascript lookup *) 
val create_class_signature_content : Html.t list -> Html.t

(** Create a class container used by the javascript inliner *) 
val create_class_container : string -> Html.t -> Html.t -> Gentyp.path option -> Html.t

