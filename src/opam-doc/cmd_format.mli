type cmd = {
  cmd_modname : string;
  cmd_sourcefile : string;
  cmd_doctree : Doctree.file;
}

val read_cmd : string -> cmd
