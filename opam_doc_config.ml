(* Options and arguments parsing *)

open Arg

(* Todo : find a proper place to put the file *)
let _index_file_path = ref ((Sys.getcwd ())^"/opam-doc.idx")
let _default_index_name = ref "index.html"
let _filter_pervasives = ref false
let _clear_index = ref false
let _always_proceed = ref false
let _package_descr = ref ""
let _current_package = ref "test"

let index_file_path () = !_index_file_path
let default_index_name () = !_default_index_name
let filter_pervasives () = !_filter_pervasives
let clear_index () = !_clear_index
let always_proceed () = !_always_proceed
let package_descr () = !_package_descr
let current_package () = !_current_package

let set_current_package p = _current_package := p

let options  = 
  [ ("--package", Set_string _current_package, "Specify the package")
  ; ("-p", Set_string _current_package, "Specify the package")
  ; ("--package-description", Set_string _package_descr, "Add a description to the package")
  ; ("-descr", Set_string _package_descr, "Add a description to the package")
      
  ; ("-index", Set_string _index_file_path, "Use a specific index file to use rather than the default one")
    
  ; ("--filter-pervasives", Set _filter_pervasives, "Remove the 'Pervasives' label to Pervasives' references")
    
  ; ("--clear-index", Set _clear_index, "Clear the global index before processing")

  ; ("-y", Set _always_proceed, "Answer yes to all questions prompted")

(*    ("-online-url", Set_string online_url, "Give the path to an online documentation, references to this library using the -online-links option will use this url");
*)
(*    ("-online-links", Set use_online_links, "Generate online references instead of locals one");
*)
  ]

let usage = "Usage: opam-doc [--package 'package_name'] <cm[dt] files>"


(* Html config *)

let doctype = "<!DOCTYPE HTML>\n"
let character_encoding =
  <:html<<meta content="text/html; charset=iso-8859-1" http-equiv="Content-Type" />&>>

let default_stylesheet = String.concat "\n"
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
    ".expander { width:1.5em; height:1.5em; border-radius:0.3em; font-weight: bold }";
    ".expanding_sig { border-spacing: 5px 1px }";
    ".expanding_sig td { vertical-align: text-top }";
    ".expanding_include_0 td { vertical-align: text-top }"; 
    ".expanding_include_1 td { vertical-align: text-top }"; 
    ".expanding_include_2 td { vertical-align: text-top }"; 
    ".expanding_include_3 td { vertical-align: text-top }"; 
    ".expanding_include_4 td { vertical-align: text-top }"; 
    ".expanding_include_5 td { vertical-align: text-top }"; 
    ".expanding_include_6 td { vertical-align: text-top }"; 
    "table.expanding_include_0, table.expanding_include_1, table.expanding_include_2, table.expanding_include_3, 
     table.expanding_include_4, table.expanding_include_5, table.expanding_include_6 
     { border-top: thin dashed; border-bottom: thin dashed; border-collapse: collapse}";
    "table.expanding_include_0 { background-color: #FFF5F5; }"; 
    "table.expanding_include_1 { background-color: #F5F5FF; }"; 
    "table.expanding_include_2 { background-color: #F5FFF5; }"; 
    "table.expanding_include_3 { background-color: #FFF5FF; }"; 
    "table.expanding_include_4 { background-color: #FFFFF5; }"; 
    "table.expanding_include_5 { background-color: #F5FFFF; }"; 
    "table.expanding_include_6 { background-color: #FFF5EB; }"; 
    "td.edge_column { border-right: 3px solid lightgrey }";
  ]


(** Marks used to generate id attributes *)
type mark = Attribute | Type | Type_elt | Function | Exception | Value | Method | Title

let jquery_online_url = "http://ajax.googleapis.com/ajax/libs/jquery/1.10.1/jquery.min.js"

let style_filename = "style.css"

let style_tag =
  <:html<<link rel="stylesheet" href="$str:style_filename$" type="text/css" />&>>


(* Ajax loading *)

let script_filename = "doc_loader.js"

let script_tag =
  <:html<<script type="text/javascript" src="$str:jquery_online_url$"> </script>
<script type="text/javascript" src="$str:script_filename$"> </script>&>>

let default_script = 
"var opamdoc_contents = 'body'

// utility - Parse query string
function parseParams(query) {
    var re = /([^&=]+)=?([^&]*)/g;
    var decodeRE = /\+/g; // Regex for replacing addition symbol with a space
    var decode = function (str) {return decodeURIComponent( str.replace(decodeRE, ' ') );};
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
}

// utility - Fetch HTML from URL using ajax
function ajax(url, cont){
    console.log('AJAX request : ' + url);
    $.ajax({
        type: 'GET',
        url:url,
        async:true,
        dataType: 'html'
    }).done(function(data){
        cont($(data));
    }).fail(function(){
        console.log('AJAX request failed : ' + url);
    });
}

function Path(pathStr){

    var args = parseParams(pathStr);

    this.package = null;
    this.module = null;
    this.subnames = [];
    this.subkinds = [];
    this.class = null;
    this.type = null;

    if(typeof args.package !== 'undefined') {
        this.package = args.package;
        if(typeof args.module !== 'undefined') {
            var modstring = args.module;
            var names = [];
            var kinds = [];
            var done = false;
            var kind = 'module'
            var i = 0;
            while(!done) {
                var dot_index = modstring.indexOf('.');
                var colon_index = modstring.indexOf(':');
                if(dot_index > 0 && (dot_index < colon_index || colon_index < 0)) {
                    names[i] = modstring.substring(0, dot_index);
                    kinds[i] = kind;
                    kind = 'module';
                    modstring = modstring.substring(dot_index + 1);
                } else if(colon_index > -1) {
                    names[i] = modstring.substring(0, colon_index);
                    kinds[i] = kind;
                    kind = 'modtype';
                    modstring = modstring.substring(colon_index + 1);
                } else {
                    names[i] = modstring;
                    kinds[i] = kind;
                    done = true;
                }
                i++;
            }
            this.module = names[0];
            if(names.length > 1) {
                this.subnames = names.splice(1);
                this.subkinds = kinds.splice(1);
            }
            if(typeof args.class !== 'undefined') {
                this.class = args.class;
            }
            if(typeof args.type !== 'undefined') {
                this.type = args.type;
            }
        } 
    }
}

Path.prototype.name = function () {
    var name = null;
    if(this.package !== null) {
        name = this.package;
        if(this.module !== null) {
            name = this.module;
            if(this.subnames.length > 0){
                name += '.' + this.subnames.join('.');
            } 
            if(this.class !== null){
                name += '.' + this.class;
            } 
        }
    }        
    return name;
}

Path.prototype.fullName = function () {
    var fullName = null;
    if(this.package !== null) {
        fullName = 'Package ' + this.package;
        if(this.module !== null) {
            var module = this.module;
            if(this.subnames.length > 0){
                module += '.' + this.subnames.join('.');
            } 
            if(this.class !== null){
                fullName = 'Class ' + module + '.' + this.class;
            } else if (this.subkinds[this.subnames.length - 1] === 'modtype') {
                fullName = 'Module type ' + module;
            } else {
                fullName = 'Module ' + module;
            }
        }
    }        
    return fullName;
}

Path.prototype.url = function () { 
    var url = null;
    if(this.package !== null) {
        url = '?package=' + this.package;
        if(this.module !== null) {
            url += '&module=' + this.module;
            for(var i = 0; i < this.subnames.length; i++) {
                if(this.subkinds[i] === 'modtype') {
                    url += ':' + this.subnames[i];
                } else {
                    url += '.' + this.subnames[i];
                }
            } 
            if(this.class !== null){
                url += '&class=' + this.class;
            }
            if(this.type !== null){
                url += '&type=' + this.type;
            }
        }
    }        
    return url;
}

function Parent(path) {
    this.package = null;
    this.module = null;
    this.subnames = [];
    this.subkinds = [];
    this.class = null;
    this.type = null;

    if(path.package !== null) {
        if(path.module !== null) {
            this.package = path.package;
            if(path.subnames.length > 0 || path.class !== null) {
                this.module = path.module;
                if(path.class !== null) {
                    this.subnames = path.subnames;
                    this.subkinds = path.subkinds;
                } else {
                    this.subnames = path.subnames.slice(0, -1);
                    this.subkinds = path.subkinds.slice(0, -1);
                }
            }
        } 
    }
}

Path.prototype.parent = function () { return new Parent(this) }

Parent.prototype = Path.prototype

function PathVisitor(path) {
    this.path = path;
    this.subnames = path.subnames.slice(0);
    this.subkinds = path.subkinds.slice(0);
    this.class = path.class
}

PathVisitor.prototype.current = function (){
    if(this.subnames.length > 0) {
        return {kind: this.subkinds[0], name: this.subnames[0]};
    } else {
        if(this.class !== null) {
            return {kind: 'class', name: this.class};
        } else {
            return null;
        }
    }
}

PathVisitor.prototype.next = function (){
    if(this.subnames.length > 0) {
        this.subnames.shift();
        this.subkinds.shift();
    } else {
      this.class = null;
    }
    return this;
}

PathVisitor.prototype.concat = function(pv){
    this.subnames = this.subnames.concat(pv.subnames);
    this.subkinds = this.subkinds.concat(pv.subkinds);
    if(pv.class !== null) {
      this.class = pv.class;
    }
    return this;
}


function Page(path){
    this.path = path;
    this.alias = null;
    this.summary = null;
    this.body = null;
    this.constraints = null;
    this.jump = null;
}

Page.prototype.parent_link = function(){
    var parent = this.path.parent();
    var title = parent.name();
    var url = parent.url();
    if(title !== null && url !== null) {
        return $('<a>', 
                 {'class' : 'up', 
                  title   : title,
                  href    : url,
                  text    : 'Up' });
    }
    return null;
}

Page.prototype.title = function(){
    var alias = null;
    var sep = '';
    if(this.alias !== null) {
        if(this.path.modtype !== null) {
          sep = ' = ';
        } else {
          sep = ' : ';
        }
        alias = $('<a>', 
                  {href    : this.alias.url(),
                   text    : this.alias.name()});
    }
     
    return $('<h1>').append(this.path.fullName() + sep).append(alias);
}

function display_page(page){
    var plink = page.parent_link();
    var title = page.title();
    var summary = page.summary;
    var rule = $('<hr/>').attr('width','100%');
    var body = page.body;

    var content = $('<div>')
        .append(plink)
        .append(title)
        .append(summary)
        .append(rule)
        .append(body);

    if(page.jump !== null) {
        scrollTo(0, page.jump.position().top);
        page.jump.css('background', 'yellow');
    }

    $(opamdoc_contents).html(content);
}

function load_page(page, pv, data, cont) {

    var current = pv.current();

    if(current === null) {
        page.summary = $('> div.info', data).first();
        page.body = data;
        if(page.path !== pv.path) {
            page.alias = pv.path
        }
        if(page.path.type !== null)
        {
            var jump = $('pre > span.TYPE'+page.path.type, data);
	    if (jump.length == 0){
	        jump = $('pre > code > span.TYPE'+page.path.type, data);
	    }
	    if (jump.length > 0){
	        page.jump = jump;
	    }
        }
        cont(page);
    } else {

        var kind = current.kind;
        var name = current.name;

        var query = '> div.ocaml_' + kind + '[name=' + name + ']'
        var subdata = $(query, data)

        if(subdata.length === 0) {

	    var includes = $('> div.ocaml_include', data);

	    for (var i = 0; i < includes.length; i++){

	        var items = JSON.parse($(includes[i]).attr('items'));

	        if (items.indexOf(name) !== -1){
		    
		    var pathAttr = $(includes[i]).attr('path');

		    if (typeof pathAttr === 'undefined'){
		        var content = $('> div.ocaml_content', includes[i]);

			load_page(page, pv, content, cont);
		    } else {
		        var include_path = new Path(pathAttr.substring(1));
                        var include_pv = new PathVisitor(path);

                        var include_url = include_path.package + '/' + include_path.module +'.html'
                        
		        ajax(include_url, function(data){
                            load_page(page, include_pv.concat(pv), data, cont);
                        });
		    }
	        }
	    }
        } else {

	    var pathAttr = subdata.attr('path');

	    if (typeof pathAttr === 'undefined'){
	        var content = $('> div.ocaml_content', subdata);
	        
	        load_page(page, pv.next(), content, cont);
	    } else {
	       
		var alias_path = new Path(pathAttr.substring(1));
                var alias_pv = new PathVisitor(alias_path);

                var alias_url = alias_path.package + '/' + alias_path.module +'.html'

		ajax(alias_url, function(data){
                    load_page(page, alias_pv.concat(pv.next()), data, cont);
                });
	    }
        }
    }
}

function load_path(path, cont) {
    if(path.module !== null) {
        var url = path.package + '/' + path.module + '.html';
        ajax(url, function(data){
            var pg = new Page(path);
            var pv = new PathVisitor(path);
            
            load_page(pg, pv, data, cont);
        });
    } else {
        var url = path.package + '/index.html';
        ajax(url, function(data){
            var pg = new Page(path);
            pg.body = data;
            cont(pg);
        });
    }
}

function Expander(expanded, button, expansion) {
    if(expanded) { 
        button.html('-');
        expansion.show();
    } else { 
        button.html('+');
        expansion.hide();
    }
    this.expanded = expanded;
    this.button = button;
    this.expansion = expansion;
}

Expander.prototype.expand = function(expand){
    if(typeof expand === 'undefined') {
        expand = ! this.expanded;
    }
    if(expand !== this.expanded) {
        this.button.html(expand ? '-' : '+');
        if(expand) {
            this.expansion.show('fast');
        } else {
            this.expansion.hide('fast');
        }
        this.expanded = expand;
    }
}

function Group(parent) {
    if(typeof parent !== 'undefined'){
        this.cls = null;
        this.content = null;
        this.path = null;
        if(parent !== null) {
            this.depth = parent.depth + 1;
            this.icount = parent.icount;
            this.auto_expand = parent.auto_expand;
        } else {
            this.depth = 0;
            this.icount = 6;
            this.auto_expand = true;
        }
    }
}

Group.prototype.load_content = function(data){
    this.load_children(data);
    this.content = data;
}

Group.prototype.load_path = function(data){
    var pathAttr = data.attr('path');
    if(typeof pathAttr !== 'undefined') {
        this.path = new Path(pathAttr.substring(1));
    }
}

Group.prototype.decorate = function(node){
    var button = $('<button>').addClass('expander');
    var btn_cell = $('<td>').append(button);
    var node_cell = $('<td>').append(node.children()).width('100%');
    var node_row = $('<tr>').append(btn_cell).append(node_cell);
    var table = $('<table>')
        .addClass('expanding_' + this.cls)
        .width('100%')
        .append(node_row);
    if(this.content !== null) {
        table.append(this.content);
        var expander = new Expander(this.auto_expand, button, this.content);
        button.click(function () { expander.expand() });
    } else if(this.path !== null) {
        button.html('+');
        var self = this;
        var expand = function(page){
            self.load_content(page.body);
            table.append(self.content);
            var expander = new Expander(self.auto_expand, button, self.content);
            button.click(function () { expander.expand() });
            expander.expand(true);
        };
        if(this.auto_expand) {
            load_path(self.path, expand);
        } else {
            button.click(function () {
                button.off('click');
                load_path(self.path, expand);
            });
        }
    } else {
        button.html('+');
        button.attr('disabled', true);
    }
    node.append(table);
}

function IncludeGroup(parent, idx) {
    Group.call(this, parent);
    this.icount = (parent.icount + idx + 2) % 7;
    this.cls = 'include_' + this.icount;
    if(this.depth > 2) {
        this.auto_expand = false;
    }
}

IncludeGroup.prototype = new Group();

IncludeGroup.prototype.load_content = function(data) {
    this.load_children(data);
    var cell = $('<td>')
        .attr('colspan', '2')
        .css('padding', 0)
        .append(data);
    this.content = $('<tr>').append(cell);
}

function SigGroup(parent, idx) {
    Group.call(this, parent);
    this.icount = (parent.icount - idx - 1) % 7;
    this.cls = 'sig';
    this.auto_expand = false;
}

SigGroup.prototype = new Group();

SigGroup.prototype.load_content = function(data) {
    this.load_children(data);
    var edge_cell = $('<td>').addClass('edge_column');
    var cnt_cell = $('<td>').append(data);
    this.content = $('<tr>').append(edge_cell).append(cnt_cell);
}

Group.prototype.load_children = function(data, Kind, label){
    if(typeof Kind === 'undefined') {
        this.load_children(data, IncludeGroup, 'include');
        this.load_children(data, SigGroup, 'module');
        this.load_children(data, SigGroup, 'modtype');
        this.load_children(data, SigGroup, 'class');
    } else {
        var children = $('> div.ocaml_' + label, data);
        var self = this;
        children.each(function(idx) {
            var grp = new Kind(self, idx);
            var content = $('div.ocaml_content', $(this));
            if(content.length > 0) {
                grp.load_content(content);
            } else {
                grp.load_path($(this));
            }
            grp.decorate($(this));
        });
    }
}

$(document).ready(function () {
    var p = new Path(location.search.substring(1));
    var grp = new Group(null);
    load_path(p, function(page){
        grp.load_content(page.body);
        display_page(page);
    });
});
"
