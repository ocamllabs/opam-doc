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
val local_lookup: local -> string -> string option

val get_global_modules: global -> string list

(* debug *)
val global_print: global -> unit
val local_print: local -> unit

val read_global_file: string -> global

val update_global: global -> string list -> global

val write_global_file : global -> string -> unit
