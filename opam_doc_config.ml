(* Options and arguments parsing *)

open Arg

(* Todo : find a proper place to put the file *)
let index_file_path = ref ((Sys.getcwd ())^"/opam-doc.idx")
let default_index_name = ref "index.html"

let online_url = ref ""

let filter_pervasives = ref false
let clear_index = ref false
let use_online_links = ref true


let current_package = ref "test"

let options  = 
  [
    ("--package", Set_string current_package, "Specify the package");

    ("-index", Set_string index_file_path, "Use a specific index file to use rather than the default one");
    
    ("--filter-pervasives", Set filter_pervasives, "Add the 'Pervasives' label to Pervasives' references");
    
    ("--clear-index", Set clear_index, "Clear the global index before processing");
    
(*    ("-online-url", Set_string online_url, "Give the path to an online documentation, references to this library using the -online-links option will use this url");
*)
    ("-online-links", Set use_online_links, "Generate online references instead of locals one");
    
  ]

    
let usage = "[options] <files>"


(* Html config *)

open Cow

let doctype = "<!DOCTYPE HTML>\n"
let character_encoding = 
  <:html<<meta content="text/html; charset=iso-8859-1" http-equiv="Content-Type" />&>>

let default_stylesheet =
  [ ".keyword { color: #f47421; font-weight: bold }";
    ".keywordsign { color: #f47421 }";
    ".superscript { font-size : 4 }";
    ".subscript { font-size : 4 }";
    ".comment { color: #747474; font-style: italic }";
    ".constructor { color: #15c17a }";
    ".type { color: #c746cc }";
    ".string { color: #09a7e2 }";
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

    (* My stuff *)
    ".expanding_content { border-left:1px solid black; padding: 5px; margin-bottom:5px }";
    ".expanding_content button { width:25px; float:left; margin:3px; }";
  ]

(** The prefix for types declaration *)
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

let content_to_load_class = "content_to_load"

let script_filename = "doc_loader.js"

let script_tag = 
  <:html<<script type="text/javascript" src="$str:jquery_online_url$"> </script>
<script type="text/javascript" src="$str:script_filename$"> </script>&>>

let page_contents_extension = ".contents"
  
let default_script = "// utility - Parse query string
(function($) {
    var re = /([^&=]+)=?([^&]*)/g;
    var decodeRE = /\\+/g; // Regex for replacing addition symbol with a space
    var decode = function (str) {return decodeURIComponent( str.replace(decodeRE, \" \") );};
    $.parseParams = function(query) {
	query = query.replace(/&amp;/g, '&'); // <= THIS FIXES THE COW HTTP ESCAPING THE &s WHEN IT SHOULDN'T
	var params = {}, e;
	while ( e = re.exec(query) ) {
	    var k = decode( e[1] ), v = decode( e[2] );
	    if (k.substring(k.length - 2) === '[]') {
		k = k.substring(0, k.length - 2);
		(params[k] || (params[k] = [])).push(v);
	    }
	    else params[k] = v;
	}
	return params;
    };
})(jQuery);

//Add event
var on = (function(){
    if (window.addEventListener) {
        return function(target, type, listener){
            target.addEventListener(type, listener, false);
        };
    }
    else {
        return function(object, sEvent, fpNotify){
            object.attachEvent(\"on\" + sEvent, fpNotify);
        };
    }
}());

// OPAM DOC SCRIPT

function create_menu(){
    var args = $.parseParams(location.search.substring(1));
    if (typeof args.module !== 'undefined' && args.module != ''){
	var module_arr = args.module.split(\".\");
	module_arr.pop();
	var parent_module = module_arr.join(\".\");
	var url = '?package='+args.package
	    +(parent_module!=''?'&module='+parent_module:\"\");
	
	return '<a class=\"up\" title=\"'+parent_module+'\" href=\"'+url+'\">Up</a>';

    }

}

//load package only
function load_package_index(package_name){
    console.log(\"Loading index : \"+package_name+\"/index.html...\");
    $(\"body\").load(package_name+\"/index.html\",
		   function(response, status, xhr){
		       $(\"body\")
			   .prepend(\"<h1>Package \"+package_name+\"</h1>\")
			   .prepend('<a href=\"?\">Package list</a>');
		   });
}

//toggle elements
function expand_content(){
    var expand = this.innerHTML == '+'?true:false;
    this.innerHTML = expand?'-':'+';
    this.parentNode.lastElementChild.style.display = expand?'block':'none';
}

// node : dom node = <div *>..</div>contents_to_be_hidden</div></div>- is_diplayed = boolean
function wrap_element(node, is_shrinked, extra_handler){
    
    node.classList.add('expanding_content');
    node.lastElementChild.style.display = is_shrinked?'none':'block';
    
    var button = document.createElement(\"button\");
    button.innerHTML = is_shrinked?'+':'-';
    button.onclick = expand_content;
    if (typeof extra_handler !== 'undefined')
	on(button, \"click\", extra_handler);
    
    node.insertBefore(button, node.firstChild);
}

function expand_includes(){
    var $mod_includes = $(document).find(\"div.ocaml_include\");
    
    if ($mod_includes.length != 0){
	$mod_includes.each(function(){
	    $(this).removeClass('ocaml_include');
	    
	    if ($(this).is(\".ident\")){
		var path = $(this).attr(\"path\");

		var args = $.parseParams(path.substring(1));
		var module_arr = args.module.split(\".\");
		
		var $data = perform_ajax_request(args.package+'/'+ module_arr[0]+'.html', false);
		var result = lookup_module($data, module_arr.slice(1));
		result.content.find(\"> div.info\").remove();
		console.log(result.content);
		
		//.wrap(\"<div></div>\").parent() doesn't work here - Oo
		var new_node = $(document.createElement(\"div\"));
		$(this).append($(new_node).append(result.content).html());
	    }
	    
	    wrap_element($(this)[0], false);
	});
	
	expand_includes(); // the processed includes could unwrap others includes
    }
}

function shrink_modules (){
    var rec_call = false;

    $(\"div.ocaml_module.sig\").each(function (){
	console.log(\"sig : \"+$(this).attr(\"name\"));
	$(this).removeClass(\"ocaml_module sig\");
	wrap_element($(this)[0], true);
    });

    $(\"div.ocaml_module.ident\").each(function (){
	console.log(\"ident : \"+$(this).attr(\"name\"));
	$(this).removeClass(\"ocaml_module ident\");
	var path = $(this).attr(\"path\");
	
	if (typeof path !== 'undefined'){
	    var args = $.parseParams(path.substring(1));
	    var module_arr = args.module.split(\".\");
	    var url = args.package+'/'+ module_arr[0]+'.html';
	    
	    var $data = perform_ajax_request(url, false);
	    
	    var result = lookup_module($data, module_arr.slice(1));
	    result.content.find(\"> div.info\").remove();

	    //.wrap(\"<div></div>\").parent() doesn't work here - Oo
	    var new_node = $(document.createElement(\"div\"));
	    $(this).append($(new_node).append(result.content).html());

	    wrap_element($(this)[0], true);
	    
	    console.log(\"rec_callED\");
	    rec_call = true; // ask for a recursion
	    
	}
    });
				     
    if (rec_call){ console.log(\"CALL\"); shrink_modules();}
    console.log(\"FINI\");
}

// Load the included modules, wraps the module content with a button and hides it
function expand_sub_nodes(){
    expand_includes();
    shrink_modules();
}

/*
  module_content => <div> (<pre>|<div class=ocaml_module>) list </div>
  page_title => B.M.X = Ext.T
  signature => <div> module a = x  <div class='info'> </div> </div>
*/
function write_content(module_content, page_title, signature){
    //Clear the page
    $(\"body\").empty();

    $(\"body\")
	.append(create_menu())
	.append(\"<h1>Module \"+page_title+\"</h1>\")
	.append(signature.html())
	.append(\"<hr width='100%'>\")
	.append(module_content.html());
    
    expand_sub_nodes();
}

function perform_ajax_request(url, async){
    console.log('doing ajax request with : '+url);
    var $data;
    $.ajax({
	type: \"GET\",
	url:url,
	async:async
    }).done(function(data_received){
	$data = $(data_received.firstChild);
	if ($data.length == 0)
	    $data = $(data_received);
    }).fail(function(){
	console.log(\"Ajax request failed on : \"+url);
    });

    return $data;
}

// Fetch first found signature
function lookup_module_rec($data, module_arr, title, signature){
    console.log(\"[DEBUG] Call - lookup_module_rec(data:\"+$data+\", module_arr:\"+module_arr+\", title:\"+title+\", signature:\"+signature)+\")\";
    
    if (module_arr.length == 0)
	return {content:$data, title:title, signature:signature};

    // else {
    var content, target_title;
    var $query = $data.find('> div.ocaml_module[name='+module_arr[0]+']');
    
    // If there are no matching elements, then it can be in an include
    if ($query.length == 0){
	// TEST IF THE ARRAY IS PARSED AS A JAVASCRIPT VALUE
	var includes = $data.find('> div.ocaml_include');
	for (var i = 0; i < includes.length; i++){
	    var item_attr = $(includes[i]).attr('items');
	    
	    if (typeof str === undefined){
		continue;
	    }
	    
	    var items = JSON.parse(item_attr);

	    console.log(\"looking for : \"+module_arr[0]);
	    
	    // if the include contains the module we are looking for :
	    if (items.indexOf(module_arr[0]) !== -1){
		
		var next_path = $(includes[i]).attr('path');
		//if this include is an anonymous declaration we do a recursion on the internal content
		if (typeof next_path === 'undefined'){
		    var include_sig_content = $(includes[i]).find(\"> div.ocaml_module_content\");
		    if (include_sig_content.length > 0){
			return lookup_module_rec(include_sig_content, module_arr, title, signature);
		    } else {
			console.error(\"this include : \"+ includes[i] +\" is fucked up -- continue..\");
			continue;
		    }
		} 
		// if there is a path then we do an ajax request on this
		else {
		    var args = $.parseParams(next_path.substring(1));

		    var alias_module_arr = args.module.split('.');
		
		    $data = perform_ajax_request(args.package+'/'+ alias_module_arr[0]+'.html', false);
		    return lookup_module_rec($data, alias_module_arr.slice(1).concat(module_arr), title, signature); 
		}
		
	    } //end of : we found the good entry somewhere
	    else {
		// not found, we continue
		continue;
	    }
	}
    }
    // There is a module we found with that name
    else {
	//check sig or ident
	//if sig, recursion with module_content ~ like the include one
	//else ident, recursion on the ajax request... ~like the include one
	
	if ($query.is(\"div.sig\")){
	    var module_sig_content =  $query.find(\"> div.ocaml_module_content\");
	    title.addSig(module_arr[0]);
	    
	    if (signature == null && module_arr.length == 1){
		signature = $query.find('> :not(div.ocaml_module_content)');
	    }
	    
	    return lookup_module_rec(module_sig_content, module_arr.slice(1), title, signature);
	} 
	else {
	    var next_path = $query.attr('path');
	    
	    if (signature == null){
		signature = $query.find('> *');
	    }
	    
	    if (typeof next_path === 'undefined'){
		console.log(\"ident + path undefined\");
		//if this is an ident and there is no path, it is very wrong
		console.log(\"Incomplete path -- Missing dependencies documentation\");
		var info_content = $query.prepend('<div class=\"failed_lookup\">Could not lookup the content, process the package\\'s dependencies</div>');
		return lookup_module_rec(info_content, [], undefined, signature);
	    } 
	    // if there is a path then we do an ajax request on this
	    else {
		console.log(\"ident + path defined\");
		console.log(\"path = \" +next_path);
		var args = $.parseParams(next_path.substring(1));
		var alias_module_arr = args.module.split('.');
		
		$data = perform_ajax_request(args.package+'/'+ alias_module_arr[0]+'.html', false);
		title.setAlias(args.package, alias_module_arr[0]);
		console.log(alias_module_arr.slice(1).concat(module_arr.slice(1)));

		return lookup_module_rec($data, alias_module_arr.slice(1).concat(module_arr.slice(1)), title, signature); 
	    }
	}
    }
    // }
}

// data : <div ..> <div include>.</> .. <div module> </> .. <pre></pre> .. </div>
// Should only be called for internal modules
function lookup_module(data, module_arr){
    var empty_title = {_package:'', _module:'', isAlias:false,
		       setAlias: function(new_package, new_module){
			   this.isAlias=true; 
			   this._package=new_package;
			   this._module = new_module;
		       },
		       addSig: function(s){
			   this._module+=\".\"+s;
		       },
		       getLinkedTitle:function(callee_package){
			   var url = '?package='+(this._package==''?callee_package:this._package)
			       +'&module='+this._module;
			   // name = package + module or name = module ****?****
			   return '<a href=\"'+url+'\">'+this._module+'</a>';
		       }
		      };
    return lookup_module_rec(data, module_arr, empty_title, null);
}

function fetch_module_content(_package, _module, _class){
    var module_arr = _module.split(\".\");
    
    var $data = perform_ajax_request(_package+\"/\"+module_arr[0]+\".html\", false);

    var content, signature, title = _module;

    // if : toplevel module
    if (module_arr.length == 1){
	//Wrapping to be able to use .html() later.
	signature = $data.find('> div.info');
	signature.remove();
	signature = signature.wrap('<div></div>').parent();
	content = $data.find('> *').wrapAll('<div></div>').parent();

    } else {
	var result = lookup_module($data, module_arr.slice(1));
	signature = result.signature.wrap('<div></div>').parent();
	console.log(result.title);
	content = result.content;
	if (typeof result.title !== 'undefined' && result.title.isAlias){
	    title += \" = \" + result.title.getLinkedTitle(_package);
	}
    }

    write_content(content, title, signature);
}

function main(_package, _module, _type, _class){
    var pack_def = typeof _package !== 'undefined' && _package != '', 
    mod_def = typeof _module !== 'undefined' && _module != '',
    typ_def = typeof _type !== 'undefined' && _type != '';
    
    if (pack_def && !mod_def){
	load_package_index(_package);
    } else if (pack_def && mod_def) {
	fetch_module_content(_package, _module, _class);
    }

    if (typ_def){
	    location.hash = \"TYPE\"+_type;
    }
}



var args = $.parseParams(location.search.substring(1));

$(document).ready(function(){
    main(args.package, args.module, args.type, args.class);
});
"
