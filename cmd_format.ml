let cmd_magic_number = "Caml2013E001"

let read_magic_number ic =
  let len_magic_number = String.length cmd_magic_number in
  let magic_number = String.create len_magic_number in
  really_input ic magic_number 0 len_magic_number;
  magic_number

type cmd = {
  cmd_modname: string;
  cmd_sourcefile: string;
  cmd_doctree: Doctree.file;
}

let save_cmd filename modname sourcefile doctree = 
  let cmd = 
    { cmd_modname = modname;
      cmd_sourcefile = sourcefile;
      cmd_doctree = doctree; }
  in
  let oc = open_out_bin filename in
    output_string oc cmd_magic_number;
    output_value oc (cmd : cmd);
    close_out oc

let read_cmd filename = 
  let ic = open_in_bin filename in
  let magic_number = read_magic_number ic in
    if magic_number <> cmd_magic_number then raise (Failure "Not a doc file");
    let cmd = (input_value ic : cmd) in
      close_in ic;
      cmd
