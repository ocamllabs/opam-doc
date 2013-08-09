(** Global and local index tables *)

(** The global table type *)
type global

(** The local table type *)
type local

(** Return the list of name and their description of all the packages processed by this global table *)
val get_global_packages: global -> (string* Cow.Html.t option) list

(** Load the values pointed by the crcs from the global table into a local table *)
val create_local: global -> (string * Digest.t) list -> local

(** Returns the doc_path or None if looking up for a Packed module. 
   Hence, you will need to lookup the module after the first dot
   (this should be improve) *)
val local_lookup: local -> ?is_class:bool -> string list -> string

(** Read the global table from a file *)
val read_global_file: string -> global

(** Update the global table with the new cmts *)
val update_global: global -> string list -> global

(** Write the global table into a file *)
val write_global_file : global -> string -> unit

(** {2 Internal references solving} *)

(** Reset the reference table *)
val reset_internal_reference_table : unit -> unit

(** [add_internal_reference id l] adds an entry in the reference table
    with the list l being the module path to this reference.
 *)
val add_internal_reference : Ident.t -> string list -> unit

(** Lookup the internal reference and return the module path associated to this entry *)
val lookup_internal_reference : Ident.t -> string list

(** Check if the package already exist in the global table *)
val package_exists : global -> string -> bool

(** [add_global_package global package_name descr] adds or replaces a package and its description in the global table *)
val add_global_package: global -> string -> string -> global
