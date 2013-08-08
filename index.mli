type global

type local

type t_value

(* deprecated
val create_global_from_files: string list -> global
*)

val global_lookup: global -> (string * Digest.t) -> t_value

val create_local: global -> (string * Digest.t) list -> local

(**
   Returns the doc_path or None if looking up for a Packed module. 
   Hence, you will need to lookup the module after the first dot
   (this should be improve)
*)
val local_lookup: local -> ?is_class:bool -> string list -> string

val get_global_packages: global -> (string* Cow.Html.t option) list

(* debug *)
val global_print: global -> unit
val local_print: local -> unit
val global_find_key: global -> string -> unit

val read_global_file: string -> global

val update_global: global -> string list -> global

val write_global_file : global -> string -> unit

(* for internal referencing support *)
val reset_internal_reference_table : unit -> unit
val add_internal_reference : Ident.t -> string list -> unit
val lookup_internal_reference : Ident.t -> string list

val package_exists : global -> string -> bool
val add_global_package: global -> string -> string -> global
