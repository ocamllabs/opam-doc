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
let _base_uri = ref "/"
let _summary = ref None

let index_file_path () = !_index_file_path
let default_index_name () = !_default_index_name
let filter_pervasives () = !_filter_pervasives
let clear_index () = !_clear_index
let always_proceed () = !_always_proceed
let package_descr () = !_package_descr
let current_package () = !_current_package
let base_uri () = !_base_uri
let summary () = !_summary

let set_current_package p = _current_package := p
let set_summary s = _summary := Some s

let options  = 
  [ ("--package", Set_string _current_package, "Specify the package")
  ; ("-p", Set_string _current_package, "Specify the package")
  ; ("--package-description", Set_string _package_descr, "Add a description to the package")
  ; ("-descr", Set_string _package_descr, "Add a description to the package")
  ; ("--base", Set_string _base_uri, "Specify the base url")
  ; ("--summary", String set_summary, "Specify the summary page")
  ; ("-index", Set_string _index_file_path, "Use a specific index file to use rather than the default one")
  ; ("--filter-pervasives", Set _filter_pervasives, "Remove the 'Pervasives' label to Pervasives' references")
  ; ("--clear-index", Set _clear_index, "Clear the global index before processing")
  ; ("-y", Set _always_proceed, "Answer yes to all questions prompted")
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
    ".indextable td.module a {text-decoration: none; display: block; width: 100%}";
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

let style_url () = base_uri () ^ "/" ^ style_filename

let style_tag () =
  <:html<<link rel="stylesheet" href="$str:style_url ()$" type="text/css" />&>>

(* Config script *)

let config_tag () = 
  <:html<<script type="text/javascript">
var ocaml_package = '$str:current_package ()$'
var ocaml_base = '$str:base_uri ()$'
</script>&>>

(* Ajax loading *)

let script_filename = "doc_loader.js"

let script_url () = base_uri () ^ "/" ^ script_filename

let script_tag () =
  <:html<<script type="text/javascript" src="$str:jquery_online_url$"> </script>
<script type="text/javascript" src="$str:script_url ()$"> </script>&>>

let default_script = 
"var opamdoc_contents = 'body'

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

function isLIdent(ident) {
    var chr = ident.charAt(0);
    return (chr !== chr.toUpperCase()
            && chr === chr.toLowerCase())
}

var url_regexp = /^(.*)\/([^/]*)\/(?:#(.*))?$/

function Path(url){

    this.package = null;
    this.module = null;
    this.subnames = [];
    this.subkinds = [];

    var match = url_regexp.exec(url);

    var base = match[1];
    var package = match[2];
    var hash = match[3];

    if(base === ocaml_base) {

      this.package = package;

      if(typeof hash !== 'undefined' && hash !== '') {
          var modstring = hash;
          var names = [];
          var kinds = [];
          var done = false;
          var sep = '.'
          var i = 0;
          while(!done) {
              var dot_index = modstring.indexOf('.');
              var colon_index = modstring.indexOf(':');
              if(dot_index > 0 && (dot_index < colon_index || colon_index < 0)) {
                  names[i] = modstring.substring(0, dot_index);
                  if(sep === ':') {
                    kinds[i] = 'modtype';
                  } else if(isLIdent(names[i])) {
                    kinds[i] = 'class';
                  } else {
                    kinds[i] = 'module';
                  }
                  sep = '.';
                  modstring = modstring.substring(dot_index + 1);
              } else if(colon_index > -1) {
                  names[i] = modstring.substring(0, colon_index);
                  if(sep === ':') {
                    kinds[i] = 'modtype';
                  } else if(isLIdent(names[i])) {
                    kinds[i] = 'class';
                  } else {
                    kinds[i] = 'module';
                  }
                  sep = ':';
                  modstring = modstring.substring(colon_index + 1);
              } else {
                  names[i] = modstring;
                  if(sep === ':') {
                    kinds[i] = 'modtype';
                  } else if(isLIdent(names[i])) {
                    kinds[i] = 'class';
                  } else {
                    kinds[i] = 'module';
                  }
                  done = true;
              }
              i++;
          }
          this.module = names[0];
          if(names.length > 1) {
              this.subnames = names.splice(1);
              this.subkinds = kinds.splice(1);
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
        }
    }        
    return name;
}

Path.prototype.url = function () { 
    var url = null;
    if(this.package !== null) {
        url = ocaml_base + '/' + this.package;
        if(this.module !== null) {
            url += '/#' + this.module;
            for(var i = 0; i < this.subnames.length; i++) {
                if(this.subkinds[i] === 'modtype') {
                    url += ':' + this.subnames[i];
                } else {
                    url += '.' + this.subnames[i];
                }
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

    if(path.package !== null) {
        if(path.module !== null) {
            this.package = path.package;
            if(path.subnames.length > 0) {
                this.module = path.module;
                this.subnames = path.subnames.slice(0, -1);
                this.subkinds = path.subkinds.slice(0, -1);
            }
        } 
    }
}

Parent.prototype = Path.prototype

Path.prototype.parent = function () { return new Parent(this) }

function PathVisitor(path) {
    this.path = path;
    this.subnames = path.subnames.slice(0);
    this.subkinds = path.subkinds.slice(0);
}

PathVisitor.prototype.current = function (){
    if(this.subnames.length > 0) {
        return {kind: this.subkinds[0], name: this.subnames[0]};
    } else {
        return null;
    }
}

PathVisitor.prototype.next = function (){
    if(this.subnames.length > 0) {
        this.subnames.shift();
        this.subkinds.shift();
    }
    return this;
}

PathVisitor.prototype.concat = function(pv){
    this.subnames = this.subnames.concat(pv.subnames);
    this.subkinds = this.subkinds.concat(pv.subkinds);
    this.path.subnames = this.path.subnames.concat(pv.subnames);
    this.path.subkinds = this.path.subkinds.concat(pv.subkinds);

    return this;
}


function Page(path, kind){
    this.path = path;
    this.kind = kind;
    this.alias = null;
    this.summary = null;
    this.body = null;
    this.constraints = null;
    this.typ = null;
}

Page.prototype.parent_link = function(){
    var parent = this.path.parent();
    var title = parent.name();
    var url = parent.url();
    if(title === null || url === null) {
        title = 'Packages List';
        url = ocaml_base + '/';
    }
    return $('<a>', 
             {'class' : 'up', 
              title   : title,
              href    : url,
              text    : 'Up' });
}

Page.prototype.title = function(){
    var name = this.path.name();
    if(this.kind === 'module') {
        fullName = 'Module ' + name;
    } else if(this.kind === 'modtype') {
        fullName = 'Module type ' + name;
    } else if(this.kind === 'class') {
        fullName = 'Class ' + name;
    } else {
        fullName = 'Package ' + name;
    }

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
     
    return $('<h1>').append(fullName + sep).append(alias);
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

    $(opamdoc_contents).html(content);
}

function show_type(typ) {
    if(typ !== null) {
        var types = $('pre > span.TYPE'+typ).filter(':visible');
        if (types.length == 0){
            types = $('pre > code > span.TYPE'+typ).filter(':visible');;
        }
        if (types.length > 0) {
            var pos = types.position().top - (window.innerHeight / 5);
            if(pos < 0) {
                pos = 0;
            }
            window.scrollTo(0, pos);
            types.css('background', 'yellow');
        }
    }
}

function load_page(page, pv, input, cont) {

    var current = pv.current();
    var data = $('> div.ocaml_content', input);

    if(current === null) {
        page.summary = $('> div.ocaml_summary', input);
        page.body = data;
        if(page.path !== pv.path) {
            page.alias = pv.path
        }
        cont(page);
    } else {

        var kind = current.kind;
        var name = current.name;

        var query = '> div.ocaml_' + kind + '[name=' + name + ']'
        var subdata = $(query, data)

        if(subdata.length === 0) {

            var try_type = (kind === 'class');

            var includes = $('> div.ocaml_include', data);

            for (var i = 0; i < includes.length; i++){

                var items = JSON.parse($(includes[i]).attr('items'));

                if (items.indexOf(name) !== -1){
                    try_type = false;

                    var pathAttr = $(includes[i]).attr('path');

                    if (typeof pathAttr === 'undefined'){
                        load_page(page, pv, includes[i], cont);
                    } else {
                        var include_path = new Path(pathAttr);
                        var include_pv = new PathVisitor(include_path);

                        var include_url = ocaml_base + '/' + include_path.package + '/' + include_path.module +'.html'
                        
                        ajax(include_url, function(data){
                            load_page(page, include_pv.concat(pv), data, cont);
                        });
                    }
                }
            }

            if(try_type) {
                var types = $('pre > span.TYPE'+name, data);
                if (types.length == 0){
                    types = $('pre > code > span.TYPE'+name, data);
                }
                if (types.length > 0){
                    page.summary = $('> div.ocaml_summary', input);
                    page.body = data;
                    page.typ = name;
                    if(page.path !== pv.path) {
                        page.alias = pv.path.parent();
                    }
                    page.path = page.path.parent();
                    cont(page);
                } else {
                    for (var i = 0; i < includes.length; i++){
                        var items = JSON.parse($(includes[i]).attr('types'));
                        if (items.indexOf(name) !== -1){
                            page.summary = $('> div.ocaml_summary', input);
                            page.body = data;
                            page.typ = name;
                            if(page.path !== pv.path) {
                                page.alias = pv.path.parent();
                            }
                            page.path = page.path.parent();
                            cont(page);
                        }
                    }
                }
            }

        } else {
            page.kind = kind;

            var pathAttr = subdata.attr('path');

            if (typeof pathAttr === 'undefined'){
                load_page(page, pv.next(), subdata, cont);
            } else {
               
                var alias_path = new Path(pathAttr);
                var alias_pv = new PathVisitor(alias_path);

                var alias_url = ocaml_base + '/' + alias_path.package + '/' + alias_path.module +'.html'

                ajax(alias_url, function(data){
                    load_page(page, alias_pv.concat(pv.next()), data, cont);
                });
            }
        }
    }
}

function load_path(path, cont) {
    if(path.module !== null) {
        var url = ocaml_base + '/' + path.package + '/' + path.module + '.html';
        ajax(url, function(data){
            var pg = new Page(path, 'module');
            var pv = new PathVisitor(path);
            
            load_page(pg, pv, data, cont);
        });
    } else {
        var url = ocaml_base + '/' + path.package + '/summary.html';
        ajax(url, function(data){
            var pg = new Page(path, 'package');
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
        this.typ = null;
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
        this.path = new Path(pathAttr);
    }
}

Group.prototype.set_type = function(typ){
    this.typ = typ;
}

Group.prototype.check_types = function(){ }

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
            if(self.auto_expand) {
                show_type(self.typ)
            }
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
    this.typ = parent.typ;
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

IncludeGroup.prototype.check_types = function(node){ 
    var types = JSON.parse(node.attr('types'));
    if (types.indexOf(this.typ) !== -1){
        this.auto_expand = true;
    }
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
            grp.check_types($(this));
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
    var url = ocaml_base + '/' + ocaml_package + '/' + location.hash;
    var p = new Path(url);
    var grp = new Group(null);
    load_path(p, function(page){
        grp.set_type(page.typ);
        grp.load_content(page.body);
        display_page(page);
        show_type(page.typ);
    });
});

$(window).on('hashchange', function () {
    var url = ocaml_base + '/' + ocaml_package + '/' + location.hash;
    var p = new Path(url);
    var grp = new Group(null);
    load_path(p, function(page){
        grp.set_type(page.typ);
        grp.load_content(page.body);
        display_page(page);
        scrollTo(0,0);
        show_type(page.typ);
    });
});
"
