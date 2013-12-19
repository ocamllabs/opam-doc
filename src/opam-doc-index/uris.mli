val package_uri : string -> Uri.t
type kind =
  | Module
  | ModType
  | Class
  | ClassType
  | Type
val uri : ?package:string -> (string * kind) list -> Uri.t

