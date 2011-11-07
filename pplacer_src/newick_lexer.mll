(* the lex file for newick parsing.
*)

(* this stuff gets evaluated automatically *)
{
  open Ppatteries
  open Newick_parser
  open Lexing

  let escapes = Str.regexp "\\\\\\(['\\\\]\\)"
  let unquote s =
    let s = String.sub s 1 ((String.length s) - 2) in
    Str.global_replace escapes "\\1" s

}

(* Unquoted labels may not contain blanks, parentheses, square brackets,
 * braces, single quotes, colons, semicolons, or commas. *)
let unquotedchar = [^ ' ' '\t' '\n' '(' ')' '[' ']' '{' '}' '\'' ':' ';' ',']
let unquotedlabel = unquotedchar+
let escape = '\\' ['\'' '\\']
let nonescape = [^ ' ' '\'']
let quotedlabel = '\'' (escape | nonescape)* '\''

rule token = parse
  | [' ' '\t']      { token lexbuf }
  | '\n'            { Sparse.incr_lineno lexbuf; token lexbuf }
  | unquotedlabel   { LABEL (Lexing.lexeme lexbuf) }
  | quotedlabel     { LABEL (unquote (Lexing.lexeme lexbuf)) }
  | ':'             { COLON }
  | ';'             { SEMICOLON }
  | ','             { COMMA }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '['             { LBRACK }
  | ']'             { RBRACK }
  | '{'             { LBRACE }
  | '}'             { RBRACE }
  | eof             { EOF }
  | _
      { raise (Sparse.parse_error_of_positions "syntax error lexing" lexbuf.lex_start_p lexbuf.lex_curr_p) }
