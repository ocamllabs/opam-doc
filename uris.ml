
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

let uri kind ?package elems = 
  let pkg = package_query package in
  let rec loop sep acc elems = 
    match elems with
      [] -> assert false
    | [n] -> 
        let mods = "&module=" ^ acc in
        let comp = 
          match kind with
            Module -> sep ^ n
          | ModType -> "&modtype=" ^ n
          | Class -> "&class=" ^ n
          | ClassType -> "&classtype=" ^ n
          | Type -> "&type=" ^ n
        in
          pkg ^ mods ^ comp
    | n :: rest -> loop "." (acc ^ sep ^ n) rest
  in
    Uri.of_string (loop "" "" elems)
