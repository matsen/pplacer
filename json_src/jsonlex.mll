{
  open Ppatteries
  open Jsonparse
  open Lexing
}

let nonzero = ['1'-'9']
let digit = ['0'-'9']
let digit_lead = '0' | nonzero digit*
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let escape = '\\' (['"' '\\' '/' 'b' 'f' 'n' 'r' 't'] | 'u' hexdigit hexdigit hexdigit hexdigit)
let nonescape = [^ '\\' '"' '\t' '\n' '\r']

rule token = parse
  | [' ' '\t' '\r']
      { token lexbuf }
  | '\n'
      { Sparse.incr_lineno lexbuf; token lexbuf }

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

  | '-'? digit_lead as i
      { INT (int_of_string i) }
  | '-'? digit_lead ('.' digit+)? (['e' 'E'] ['+' '-']? digit+)? as f
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
        raise (Sparse.parse_error_of_positions "syntax error lexing" lexbuf.lex_start_p lexbuf.lex_curr_p)
      }
