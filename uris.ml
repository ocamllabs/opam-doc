
let package_query package = 
  "?package="^ match package with
    | None -> Opam_doc_config.current_package ()
    | Some p -> p
  
let package_uri name =
  Uri.of_string (package_query (Some name))

type kind =
  | Module
  | ModType
  | Class
  | ClassType
  | Type

let uri ?package elems = 
  let pkg = package_query package in
  let rec loop sep acc elems = 
    match elems with
      [] -> assert false
    | [n, kind] -> 
        let mods = "&module=" ^ acc in
        let comp = 
          match kind with
            Module -> if sep then "." ^ n else n
          | ModType -> if sep then ":" ^ n else n
          | Class -> "&class=" ^ n
          | ClassType -> "&classtype=" ^ n
          | Type -> "&type=" ^ n
        in
          pkg ^ mods ^ comp
    | (n, kind) :: rest -> 
        let acc = 
          match kind with
            Module -> if sep then acc ^ "." ^ n else acc ^ n
          | ModType -> if sep then acc ^ ":" ^ n else acc ^ n
          | _ -> assert false
        in
          loop true acc rest
  in
    Uri.of_string (loop false "" elems)
