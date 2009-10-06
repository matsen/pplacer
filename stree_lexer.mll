(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.

*)

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


