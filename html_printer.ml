open Index
open Generate
open Cow

open Docjson

let default_stylesheet = "style.css"

let keyword str = "<span class=\"keyword\">"^str^"</span>"

let info str = "<div class=\"info\">"^str^"</div><br>"

let code clazz str = "<code class=\""^clazz^"\">"^str^"</code>"
  
let html_of_params params variance =
  let lstrparam = (List.map2 
		     (fun param variance -> 
		       (match variance with 
			   `None -> "" 
			 | `Positive -> "+" 
			 | `Negative -> "-")
		       ^ (string_of_html param)) 
		     params variance) in
  let rec insert_between str = function
    | [] -> ""
    | [h] -> h
    | h::t -> h^str^(insert_between str t) in
  code "type"
    (match lstrparam with
      [] -> ""
    | [h] -> h^" "
    | _ -> "("^(insert_between ", " lstrparam)^") ")
    
let html_of_Value = function
  | { item = `Value;
      name = Some name;
      typ = Some typ;
      primitive = None;
      params = None;
      cstrs = None;
      type_kind = None;
      priv = None;
      manifest = None;
      variance = None;
      args = None;
      module_type = None;
      virt = None;
      class_type = None;
      _ } as elem ->
    (keyword "val")^" "^name^" : "^(code "type" (string_of_html typ))^
      (match (get_info_sig_item elem) with Some i -> info (string_of_html i) | None -> "")
    ^"\n"
  | _ -> assert false
    
(*
(* primitive? *)
  let html_of_Primitive = function
  | { item = `Primitive;
      name = Some name;
      typ = Some typ;
      primitive = Some primitive;
      params = None;
      cstrs = None;
      type_kind = None;
      priv = None;
      manifest = None;
      variance = None;
      args = None;
      module_type = None;
      virt = None;
      class_type = None;
      _ } as elem ->
    ""
  | _ -> assert false
*)


let html_of_variant_constructor e =
  let (name, typs, inf) = triplet_of_constructor e in
  name
  ^(if typs != [] then " "^(keyword "of")^" " else "")
  ^(String.concat " * " (List.map string_of_html typs))
  ^(match inf with None -> "<br/>" | Some i -> info (string_of_html i))

let html_of_variant_constructor_list = function 
  | Some l -> List.fold_left 
    (fun acc e -> acc^(keyword "| ")^(html_of_variant_constructor e))
    "= <br/>" 
    l
  | None -> ""

let html_of_record_label e =
  match record_fields e with
    | name,mut,typ,inf -> 
      "<tr>"
      ^"<td align=\"left\" valign=\"top\">"
      ^(if mut then (keyword "mutable ")^"</td>" else "&nbsp;&nbsp;</td>")
      ^"<td align=\"left\" valign=\"top\">"
      ^name
      ^" :</td>"
      ^"<td align=\"left\" valign=\"top\">"^(code "type" (string_of_html typ))
      ^(match inf with None -> "<br/>" | Some i -> info (string_of_html i))
      ^"</td></tr>"

let html_of_record_label_list : Docjson.Record_label.record_label list option -> string  = function 
  | Some( l:Docjson.Record_label.record_label list) -> 
    "{<br>"
    ^"<table class=\"typetable\">"
    ^(String.concat "<br>" (List.map html_of_record_label l))
    ^"</table>}"    
  | None -> ""

let html_of_typekind (type_kind:Docjson.type_kind) =
  match type_kind.kind with
      `Abstract -> ""
    | `Variant -> html_of_variant_constructor_list type_kind.constructors
    | `Record -> html_of_record_label_list type_kind.labels

(* todo : constraints *)
let html_of_Type = function
  | { item = `Type;
    name = Some name;
    typ = None;
    primitive = None;
    params = Some params;
    cstrs = cstrs; (* TODO *)
    type_kind = Some type_kind;
    priv = Some priv;
    manifest = manifest;
    variance = Some variance;
    args = None;
    module_type = None;
    virt = None;
    class_type = None;
    _ } as elem ->
(*    (* debug *)
    print_endline "*****";
    print_endline name;
    print_endline (match type_kind.kind with `Abstract -> "abs" | `Variant -> "variant" | `Record -> "record");
    Printf.printf "private? %B\n%!" priv;
    print_endline ("MANIFEST : "^(match manifest with Some m -> string_of_html m | None -> "no manifest"));
*)
    (keyword "type")
    ^" "
    ^(html_of_params params variance)
    ^name
    ^(match manifest with Some m -> " = "^(code "type" (string_of_html m)) | None -> "")
    ^(html_of_typekind type_kind)
    ^"<br>\n"
    ^ (match (get_info_sig_item elem) with Some i -> info (string_of_html i) | None -> "")
  | _ -> assert false


let generate_signature_item f item = 
  try 
  match item.item with
    | `Value -> f (html_of_Value item)
    | `Primitive -> raise (Failure "Not Supported")
    | `Type -> f (html_of_Type item)
    | `Exception -> raise (Failure "Not Supported")
    | `Module  -> raise (Failure "Not Supported")
    | `ModType -> raise (Failure "Not Supported")
    | `Include -> raise (Failure "Not Supported")
    | `Class -> raise (Failure "Not Supported")
    | `ClassType -> raise (Failure "Not Supported")
    | `Comment -> raise (Failure "Not Supported")
  with
      _ -> ()

let generate_file f = function 
  | {items=siglist; info=info} ->
    (match info with Some i -> f ("<p>"^(string_of_html i)^"</p><hr width=\"100%\">") 
      | None -> ());
    List.iter (generate_signature_item f) siglist    
    
let generate_html_header f title =
  f
    (
      "<!DOCTYPE HTML>
<html>
<head>
<title>"^title^"</title>
<link rel=\"stylesheet\" type=\"text/css\" href=\""^default_stylesheet^"\">
</head>
<body>
<h1>module "^title^"</h1>
")
    
let generate_html_footer f =
  f "</body>
</html>"
    
let generate_html ~filename:filename ~jfile:jintf =
  let oc = open_out filename in
  begin
    let buf = Buffer.create 2048 in
    let f str = Buffer.add_string buf str in
    (*    let f str = print_endline str in (* debug use *)*)
(*    print_string "Generating html...";*)
    let module_name = String.capitalize 
      (Filename.chop_extension (Filename.basename filename)) in
    generate_html_header f module_name;
    generate_file f jintf; (*output_string oc (json_to_string json);*)
    generate_html_footer f;
    output_string oc (Buffer.contents buf);
    close_out oc;
  end
    
  
    
