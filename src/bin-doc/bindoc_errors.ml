
type parser_error = 
  Unclosed of Location.t * string * string
| Expecting of string

type lexer_error = 
  Unmatched_verbatim
| Unmatched_target
| Unmatched_code
| Unmatched_pre_code
| Unmatched_html_code
| Unterminated_verbatim
| Unterminated_target
| Unterminated_code
| Unterminated_pre_code
| Unterminated_ref
| Unterminated_html_code
| Nested_verbatim
| Nested_target
| Nested_pre_code
| Nested_html_code
| Expected_see
| Unterminated_see_url
| Unterminated_see_file
| Unterminated_see_doc
| Expected_ident
| Expected_name
| Expected_version
| Expected_exception

type comment_error =
  Unterminated_simple
| Unterminated_special

type error =
  Lexer of lexer_error
| Parser of parser_error
| Comments of comment_error

exception Error of error * Location.t

open Format

let report_lexer_error ppf loc = function
  | Unmatched_verbatim ->
      fprintf ppf "%aSyntax error: unmatched 'v}'@."
          Location.print_error loc
  | Unmatched_target ->
      fprintf ppf "%aSyntax error: unmatched '%%}'@."
          Location.print_error loc
  | Unmatched_code ->
      fprintf ppf "%aSyntax error: unmatched ']'@."
          Location.print_error loc
  | Unmatched_pre_code ->
      fprintf ppf "%aSyntax error: unmatched ']}'@."
          Location.print_error loc
  | Unmatched_html_code ->
      fprintf ppf "%aSyntax error: unmatched '</code>'@."
          Location.print_error loc
  | Unterminated_verbatim ->
      fprintf ppf "%aSyntax error: unterminated '{v'@."
          Location.print_error loc
  | Unterminated_target ->
      fprintf ppf "%aSyntax error: unterminated '{%%'@."
          Location.print_error loc
  | Unterminated_code ->
      fprintf ppf "%aSyntax error: unterminated '['@."
          Location.print_error loc
  | Unterminated_pre_code ->
      fprintf ppf "%aSyntax error: unterminated '{['@."
          Location.print_error loc
  | Unterminated_ref ->
      fprintf ppf "%aSyntax error: unterminated '{!'@."
          Location.print_error loc
  | Unterminated_html_code ->
      fprintf ppf "%aSyntax error: unterminated '<code>'@."
          Location.print_error loc
  | Nested_verbatim ->
      fprintf ppf "%aSyntax error: nested '{v'@."
          Location.print_error loc
  | Nested_target ->
      fprintf ppf "%aSyntax error: nested '{%%'@."
          Location.print_error loc
  | Nested_pre_code ->
      fprintf ppf "%aSyntax error: nested '{['@."
          Location.print_error loc
  | Nested_html_code ->
      fprintf ppf "%aSyntax error: nested '<code>'@."
          Location.print_error loc
  | Expected_see ->
      fprintf ppf "%aSyntax error: expected < url >, 'filename' or \"document\"@."
          Location.print_error loc
  | Unterminated_see_url ->
      fprintf ppf "%aSyntax error: unterminated url@."
          Location.print_error loc
  | Unterminated_see_file ->
      fprintf ppf "%aSyntax error: unterminated filename@."
          Location.print_error loc
  | Unterminated_see_doc ->
      fprintf ppf "%aSyntax error: unterminated document name@."
          Location.print_error loc
  | Expected_ident ->
      fprintf ppf "%aSyntax error: expected identifier@."
          Location.print_error loc
  | Expected_name ->
      fprintf ppf "%aSyntax error: expected author name@."
          Location.print_error loc
  | Expected_version ->
      fprintf ppf "%aSyntax error: expected version string@."
          Location.print_error loc
  | Expected_exception ->
      fprintf ppf "%aSyntax error: expected exception name@."
          Location.print_error loc

let report_parser_error ppf loc = function
  | Unclosed(opening_loc, opening, closing) ->
      fprintf ppf "%aSyntax error: '%s' expected@."
        Location.print_error loc closing;
      fprintf ppf "%aThis '%s' might be unmatched@."
        Location.print_error opening_loc opening
  | Expecting nonterm ->
      fprintf ppf
        "%aSyntax error: %s expected@."
        Location.print_error loc nonterm

let report_comments_error ppf loc = function
  | Unterminated_simple ->
      fprintf ppf "%aSyntax error: unterminated comment@."
          Location.print_error loc
  | Unterminated_special ->
      fprintf ppf "%aSyntax error: unterminated special comment@."
          Location.print_error loc

let report_error ppf loc = function
  | Lexer e -> report_lexer_error ppf loc e
  | Parser e -> report_parser_error ppf loc e
  | Comments e -> report_comments_error ppf loc e
  
