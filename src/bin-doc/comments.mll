{
(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** The lexer for special comments. *)

open Lexing
open Bindoc_errors

(* The type of comments returned by the lexer*)

type t =
  Special of string * Lexing.position
| Simple
| Blank_line
| Stop

(* Update the current location with file name and line number. *)

let update_loc lexbuf loc =
  lexbuf.lex_curr_p <- loc

let incr_line ?(lines = 1) ?(chars = 0) lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- 
    { lexbuf.lex_curr_p with
      pos_lnum = pos.pos_lnum + lines;
      pos_bol = pos.pos_cnum - chars; }

(* To buffer verbatim strings and code sections *)

let string_buffer = Buffer.create 32

let reset_string_buffer () = 
  Buffer.reset string_buffer

let buffer_char c = 
  Buffer.add_char string_buffer c

let buffer_string s =
  Buffer.add_string string_buffer s

let buffer_lexeme lexbuf = 
  let s = Lexing.lexeme lexbuf in
  Buffer.add_string string_buffer s

let get_buffered_string () = Buffer.contents string_buffer

(** The locations of nested comments *)
let comment_start_locs = ref [];;

(* Count number of newlines in a string *)
let count_newlines str =
  let lines = ref 0 in
  let do_char c = if c = '\n' then incr lines in
    String.iter do_char str;
    !lines

}

let newline = ('\010' | "\013\010" )
let blank = [' ' '\009' '\012']

(* Characters that do not start a newline or comment token *)
let safe = [^ '(' '*' '\010' '\013' ]


rule main = parse
| newline
    { incr_line lexbuf;
      main lexbuf }
| newline blank* newline
    { incr_line ~lines:2 lexbuf;
      Some Blank_line }
| newline blank* newline ((blank* newline)+ as extra)
    { let lines = 2 + (count_newlines extra) in
      incr_line ~lines lexbuf;
      Some Blank_line }
| "(**/**)"
    { Some Stop }
| "(**)"
    { Some (Special("", lexbuf.lex_curr_p)) } 
| "(**" ('*'+) ")"
    { Some Simple }
| "(**" ('*'+)
    { comment_start_locs := [Location.curr lexbuf];
      simple_comment lexbuf }
| "(**"
    { comment_start_locs := [Location.curr lexbuf];
      reset_string_buffer ();
      special_comment lexbuf }
| eof
    { None }
| "(*"
    { comment_start_locs := [Location.curr lexbuf];
      simple_comment lexbuf }
| safe+ | _
    { main lexbuf }


and simple_comment = parse
| "*)"
    { match !comment_start_locs with
        [] -> assert false
      | [l] ->
          comment_start_locs := [];
          Some Simple
      | _ :: rest ->
          comment_start_locs := rest;
          simple_comment lexbuf }
| "(*"
    { comment_start_locs := 
        (Location.curr lexbuf) :: !comment_start_locs;
      simple_comment lexbuf }
| newline
    { incr_line lexbuf;
      simple_comment lexbuf }
| eof
    { match !comment_start_locs with
        [] -> assert false
      | l:: _ -> raise (Error (Comments Unterminated_simple, l)) }
| safe+ | _
    { simple_comment lexbuf }


and special_comment = parse
| "*)"
    { match !comment_start_locs with
        [] -> assert false
      | [l] ->
          comment_start_locs := [];
          let s = get_buffered_string () in
            Some (Special(s, l.Location.loc_end))
      | _ :: rest ->
          comment_start_locs := rest;
          buffer_lexeme lexbuf;
          special_comment lexbuf }
| "(*"
    { comment_start_locs := 
        (Location.curr lexbuf) :: !comment_start_locs;
      buffer_lexeme lexbuf;
      special_comment lexbuf }
| newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      special_comment lexbuf }
| (newline as n) (blank* as blanks) ("*)" as token)
    { incr_line ~chars:((String.length blanks) + 2) lexbuf;
      match !comment_start_locs with
        [] -> assert false
      | [l] ->
          comment_start_locs := [];
          (* TODO Keep the blanks as well if "-stars" is not given *)
          buffer_string n;
          let s = get_buffered_string () in
            Some (Special(s, l.Location.loc_end))
      | _ :: rest ->
          comment_start_locs := rest;
          (* TODO Keep the blanks as well if "-stars" is not given *)
          buffer_string n;
          buffer_string token;
          special_comment lexbuf }
| (newline as n) ((blank* '*') as extra)
    { incr_line ~chars:(String.length extra) lexbuf;
      (* TODO Keep the extra as well if "-stars" is not given *)
      buffer_string n;
      special_comment lexbuf }
| eof
    { match !comment_start_locs with
        [] -> assert false
      | l:: _ -> raise (Error (Comments Unterminated_special, l)) }
| safe+ | _
    { buffer_lexeme lexbuf;
      special_comment lexbuf }

{
  let lex loc s =
    let lexbuf = Lexing.from_string s in
    update_loc lexbuf loc;
    let rec loop acc = 
      match main lexbuf with
        Some c -> loop (c :: acc)
      | None -> List.rev acc
    in
      loop []
}
