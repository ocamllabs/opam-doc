
let package_query package = 
  "?package="^ match package with
    | None -> Opam_doc_config.current_package ()
    | Some p -> p
  
let module_uri ?package name =
  Uri.of_string (package_query package ^"&module="^ name)

let package_uri name =
  Uri.of_string (package_query (Some name))

let class_uri ?package modpath name =
  Uri.of_string (
    package_query package
    ^"&module="^ modpath
    ^"&class="^ name
  )

let type_uri ?package modpath name =
  Uri.of_string (
    package_query package
    ^"&module="^ modpath
    ^"&type="^ name
  )
