val package_uri : string -> Uri.t
type kind =
  | Module
  | ModType
  | Class
  | ClassType
  | Type
val uri : kind -> ?package:string -> string list -> Uri.t

