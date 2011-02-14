%{
  open Jsontype

  let syntax_error tok msg =
    raise (parse_error_of_positions msg (Parsing.rhs_start_pos tok) (Parsing.rhs_end_pos tok))

  let escapes = Str.regexp "\\\\\\([\"\\\\/bfnrt]\\|u....\\)"
  let unquote tok s =
    let s = String.sub s 1 ((String.length s) - 2) in
    Str.global_substitute escapes (function
      | "\\\\" -> "\\"
      | "\\\"" -> "\""
      | "\\/" -> "/"
      | "\\b" -> "\b"
      | "\\f" -> "\012"
      | "\\n" -> "\n"
      | "\\r" -> "\r"
      | "\\t" -> "\t"
        (* no unicode escapes just yet. *)
      | _ -> syntax_error tok "no unicode escapes"
    ) s

  let add_to_hash h (s, v) = Hashtbl.add h s v; h
%}

%token <string> STRING
%token <int> INT
%token <float> FLOAT
%token QUOTE OBRACE CBRACE OBRACK CBRACK COLON COMMA TRUE FALSE NULL EOF

%start parse
%type <Jsontype.jsontype> parse

%%

pair: STRING COLON value
      { (unquote 1 $1), $3 }

object_content:
    pair COMMA object_content
      { add_to_hash $3 $1 }
  | pair
      { add_to_hash (Hashtbl.create 16) $1 }

array_content:
    value COMMA array_content
      { $1 :: $3 }
  | value
      { [$1] }

value:
  | STRING
      { String (unquote 1 $1) }
  | INT
      { Int $1 }
  | FLOAT
      { Float $1 }

  | OBRACE object_content CBRACE
      { Object $2 }
  | OBRACE CBRACE
      { Object (Hashtbl.create 0) }
  | OBRACK array_content CBRACK
      { Array (Array.of_list $2) }
  | OBRACK CBRACK
      { Array [||] }

  | TRUE
      { Bool true }
  | FALSE
      { Bool false }
  | NULL
      { Null }

  | OBRACE error CBRACE
      { syntax_error 2 "syntax error in object" }
  | OBRACK error CBRACK
      { syntax_error 2 "syntax error in array" }

parse:
    value EOF
      { $1 }
  | error EOF
      { syntax_error 1 "syntax error parsing" }
