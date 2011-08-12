(* the lex file for newick parsing.
*)

(* this stuff gets evaluated automatically *)
{
  open Ppatteries
  open Newick_parser
  open Lexing

  let dequote s =
    let len = String.length s in
    assert(s.[0] = '\'' && s.[len-1] = '\'');
    String.sub s 1 (len-2)

  let untag s =
    let start = if s.[1] = 'I' then 2 else 1 in
    int_of_string (String.sub s start ((String.length s) - start - 1))

}

let colon = ':'
let semicolon = ';'
let comma = ','
let openp = '('
let closep = ')'
let digit = ['0'-'9']
let exponent = ['e' 'E'] ['+' '-']? digit+
let floating = (digit+ '.' digit* | digit* '.' digit+ | digit+) exponent?
(* Unquoted labels may not contain blanks, parentheses, square brackets,
 * single_quotes, colons, semicolons, or commas. *)
let unquotedchar = [^ ' ' '\t' '\n' '(' ')' '[' ']' '\'' ':' ';' ',']
let unquotedlabel = unquotedchar+
let quotedchar = [^ ' ' '\'']
let quotedlabel = '\'' quotedchar+ '\''
let edgelabel = '[' 'I'? digit+ ']'

rule token = parse
  | [' ' '\t']      { token lexbuf }
  | '\n'            { Sparse.incr_lineno lexbuf; token lexbuf }
  (* because taxnames can be floats, we have to have float first *)
  | floating        { REAL(float_of_string(Lexing.lexeme lexbuf)) }
  | unquotedlabel   { LABEL(Lexing.lexeme lexbuf) }
  | quotedlabel     { LABEL(dequote(Lexing.lexeme lexbuf)) }
  | edgelabel       { EDGE_LABEL(untag(Lexing.lexeme lexbuf)) }
  | colon           { COLON }
  | semicolon       { SEMICOLON }
  | comma           { COMMA }
  | openp           { OPENP }
  | closep          { CLOSEP }
  | eof             { EOF }
  | _
      { raise (Sparse.parse_error_of_positions "syntax error lexing" lexbuf.lex_start_p lexbuf.lex_curr_p) }
