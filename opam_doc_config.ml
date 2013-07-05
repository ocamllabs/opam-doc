(* Options and arguments parsing *)

open Arg

(* Todo : find a cool place to put the file *)
let index_file_path = ref ((Sys.getcwd ())^"/opam-doc.idx")
let output_directory = ref "./"
let online_url = ref ""


let filter_pervasives = ref true
let clear_index = ref false
let use_online_links = ref true


let options  = 
  [
    
    ("-d", Set_string output_directory, "Generate the documentation in the given directory, rather than the current directory");
    
    ("-index", Set_string index_file_path, "Use a specific index file to use rather than the default one");
    
    ("--filter_pervasives", Clear filter_pervasives, "Add the 'Pervasives' label to Pervasives' references");
    
    ("--clear-index", Set clear_index, "Clear the global index before processing");
    
    ("-online-url", Set_string online_url, "Give the path to an online documentation, references to this library using the -online-links option will use this url");
    
    ("-online-links", Set use_online_links, "Generate online references instead of locals one");
    
  ]

    
let usage = "[options] <files>"


(* Html config *)

open Cow

let doctype = "<!DOCTYPE HTML>\n"
let character_encoding = 
  <:html<<meta content="text/html; charset=iso-8859-1" http-equiv="Content-Type" />&>>

let default_stylesheet =
  [ ".keyword { font-weight : bold ; color : Red }" ;
    ".keywordsign { color : #C04600 }" ;
    ".superscript { font-size : 4 }" ;
    ".subscript { font-size : 4 }" ;
    ".comment { color : Green }" ;
    ".constructor { color : Blue }" ;
    ".type { color : #5C6585 }" ;
    ".string { color : Maroon }" ;
    ".warning { color : Red ; font-weight : bold }" ;
    ".info { margin-left : 3em; margin-right: 3em }" ;
    ".param_info { margin-top: 4px; margin-left : 3em; margin-right : 3em }" ;
    ".code { color : #465F91 ; }" ;
    ".typetable { border-style : hidden }" ;
    ".paramstable { border-style : hidden ; padding: 5pt 5pt}" ;
    "tr { background-color : White }" ;
    "td.typefieldcomment { background-color : #FFFFFF ; font-size: smaller ;}" ;
    "div.sig_block {margin-left: 2em}" ;
    "*:target { background: yellow; }" ;

    "body {font: 13px sans-serif; color: black; text-align: left; padding: 5px; margin: 0}";

    "h1 { font-size : 20pt ; text-align: center; }" ;

    "h2 { font-size : 20pt ; border: 1px solid #000000; "^
      "margin-top: 5px; margin-bottom: 2px;"^
      "text-align: center; background-color: #90BDFF ;"^
      "padding: 2px; }" ;

    "h3 { font-size : 20pt ; border: 1px solid #000000; "^
      "margin-top: 5px; margin-bottom: 2px;"^
      "text-align: center; background-color: #90DDFF ;"^
      "padding: 2px; }" ;

    "h4 { font-size : 20pt ; border: 1px solid #000000; "^
      "margin-top: 5px; margin-bottom: 2px;"^
      "text-align: center; background-color: #90EDFF ;"^
      "padding: 2px; }" ;

    "h5 { font-size : 20pt ; border: 1px solid #000000; "^
      "margin-top: 5px; margin-bottom: 2px;"^
      "text-align: center; background-color: #90FDFF ;"^
      "padding: 2px; }" ;

    "h6 { font-size : 20pt ; border: 1px solid #000000; "^
      "margin-top: 5px; margin-bottom: 2px;"^
      "text-align: center; background-color: #90BDFF ; "^
      "padding: 2px; }" ;

    "div.h7 { font-size : 20pt ; border: 1px solid #000000; "^
      "margin-top: 5px; margin-bottom: 2px;"^
      "text-align: center; background-color: #E0FFFF ; "^
      "padding: 2px; }" ;

    "div.h8 { font-size : 20pt ; border: 1px solid #000000; "^
      "margin-top: 5px; margin-bottom: 2px;"^
      "text-align: center; background-color: #F0FFFF ; "^
      "padding: 2px; }" ;

    "div.h9 { font-size : 20pt ; border: 1px solid #000000; "^
      "margin-top: 5px; margin-bottom: 2px;"^
      "text-align: center; background-color: #FFFFFF ; "^
      "padding: 2px; }" ;

    "a {color: #416DFF; text-decoration: none}";
    "a:hover {background-color: #ddd; text-decoration: underline}";
    "pre { margin-bottom: 4px; font-family: monospace; }" ;
    "pre.verbatim, pre.codepre { }";

    ".indextable {border: 1px #ddd solid; border-collapse: collapse}";
    ".indextable td, .indextable th {border: 1px #ddd solid; min-width: 80px}";
    ".indextable td.module {background-color: #eee ;  padding-left: 2px; padding-right: 2px}";
    ".indextable td.module a {color: 4E6272; text-decoration: none; display: block; width: 100%}";
    ".indextable td.module a:hover {text-decoration: underline; background-color: transparent}";
    ".deprecated {color: #888; font-style: italic}" ;

    ".indextable tr td div.info { margin-left: 2px; margin-right: 2px }" ;

    "ul.indexlist { margin-left: 0; padding-left: 0;}";
    "ul.indexlist li { list-style-type: none ; margin-left: 0; padding-left: 0; }";
  ]

let mark_type = "TYPE"
  
(** The prefix for types elements (record fields or constructors). *)
let mark_type_elt = "TYPEELT"
  
(** The prefix for functions marks. *)
let mark_function = "FUN"

(** The prefix for exceptions marks. *)
let mark_exception = "EXCEPTION"

(** The prefix for values marks. *)
let mark_value = "VAL"

(** The prefix for attributes marks. *)
let mark_attribute = "ATT"

(** The prefix for methods marks. *)
let mark_method = "METHOD"

let jquery_online_url = "http://ajax.googleapis.com/ajax/libs/jquery/1.10.1/jquery.min.js"

let style_filename = "style.css"

let style_tag = 
  <:html<<link rel="stylesheet" href="$str:style_filename$" type="text/css" />&>>


(* Ajax loading *)

let content_to_load_onclick_class = "wait_to_load"
let content_to_load_class = "content_to_load"

let default_script = 
  "function load_content (){
    $(document).ready(function(){
        var nb_elem = $('div."^content_to_load_class^"').toArray().length;
	$('div."^content_to_load_class^"').each(function(){
            $(this).attr('class', 'module_loaded');
	    $(this).load($(this).attr('file'),
			 function(){ if (--nb_elem == 0){ load_content() } });
	});
    })
}
load_content();"

let script_filename = "doc_loader.js"

let script_tag = 
  <:html<<script type="text/javascript" src="$str:jquery_online_url$"> </script>
<script type="text/javascript" src="$str:script_filename$"> </script>&>>

let page_contents_extension = ".contents"

