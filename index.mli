type global

type local

val create_global_from_files: string list -> global

val global_lookup: global -> (string * Digest.t) -> string

val create_local: global -> (string * Digest.t) list -> local

val local_lookup: local -> string -> string



val global_print: global -> unit
