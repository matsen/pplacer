(* this stuff gets evaluated automatically *)
{
  open Stree_parser
  let line = ref 1

  let dequote s = 
    let len = String.length s in
    assert(s.[0] = '\'' && s.[len-1] = '\'');
    String.sub s 1 (len-2)

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
let comment = '[' [^ ' ' '\t' '\n' '[' ']']* ']'

rule token = parse
  | [' ' '\t']      { token lexbuf }
  | comment         { token lexbuf }
  | '\n'            { incr line; CR }
  (* because taxnames can be floats, we have to have float first *)
  | floating        { REAL(Lexing.lexeme lexbuf) }
  | unquotedlabel   { LABEL(Lexing.lexeme lexbuf) }
  | quotedlabel     { LABEL(dequote(Lexing.lexeme lexbuf)) }
  | colon           { COLON }
  | semicolon       { SEMICOLON }
  | comma           { COMMA }
  | openp           { OPENP }
  | closep          { CLOSEP }
  | eof             { EOF }
  | _               { failwith ("Problem reading tree at line "^(string_of_int !line)) }


