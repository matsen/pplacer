{
  open Jsonparse
  open Jsontype
  open Lexing

  let incr_lineno lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }
}

let digit = ['0'-'9']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let escape = '\\' (['"' '\\' '/' 'b' 'f' 'n' 'r' 't'] | 'u' hexdigit hexdigit hexdigit hexdigit)
let nonescape = [^ '\\' '"']

rule token = parse
  | [' ' '\t' '\r']
      { token lexbuf }
  | '\n'
      { incr_lineno lexbuf; token lexbuf }

  | '{'
      { OBRACE }
  | '}'
      { CBRACE }
  | '['
      { OBRACK }
  | ']'
      { CBRACK }
  | ':'
      { COLON }
  | ','
      { COMMA }

  | '"' (escape | nonescape)* '"' as s
      { STRING s }

  | '-'? digit+ as i
      { INT (int_of_string i) }
  | '-'? digit+ ('.' digit+)? (['e' 'E'] ['+' '-']? digit+)? as f
      { FLOAT (float_of_string f) }

  | "true"
      { TRUE }
  | "false"
      { FALSE }
  | "null"
      { NULL }

  | eof
      { EOF }
  | _
      {
        raise (parse_error_of_positions "syntax error lexing" lexbuf.lex_start_p lexbuf.lex_curr_p)
      }
