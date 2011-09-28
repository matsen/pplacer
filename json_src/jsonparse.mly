%{
  open Ppatteries
  open Jsontype

  let utf8_encode x =
    let x' = Int32.of_int x
    and (&-) = Int32.logand
    and (|-) = Int32.logor
    and (>>-) = Int32.shift_right
    and chr i = Char.chr (Int32.to_int i)
    in
    if x <= 0x7f then
      let s = String.create 1 in
      s.[0] <- chr x';
      s
    else if x <= 0x7ff then
      let s = String.create 2 in
      s.[0] <- chr (x' >>- 6 &- 0b00011111l |- 0b11000000l);
      s.[1] <- chr (x' &- 0b00111111l |- 0b10000000l);
      s
    else if x <= 0xffff then
      let s = String.create 3 in
      s.[0] <- chr (x' >>- 12 &- 0b00001111l |- 0b11100000l);
      s.[1] <- chr (x' >>- 6 &- 0b00111111l |- 0b10000000l);
      s.[2] <- chr (x' &- 0b00111111l |- 0b10000000l);
      s
    else
      invalid_arg "utf8_encode"

  let escapes = Str.regexp "\\\\\\([\"\\\\/bfnrt]\\|u\\(....\\)\\)"
  let unquote s =
    let s = String.sub s 1 ((String.length s) - 2) in
    Str.global_substitute escapes (fun s ->
      match Str.replace_matched "\\1" s with
        | "\\" -> "\\"
        | "\"" -> "\""
        | "/" -> "/"
        | "b" -> "\b"
        | "f" -> "\012"
        | "n" -> "\n"
        | "r" -> "\r"
        | "t" -> "\t"
        | _ ->
          let escape = Str.replace_matched "\\2" s in
          utf8_encode (int_of_string ("0x" ^ escape))
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
      { (unquote $1), $3 }

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
      { String (unquote $1) }
  | INT
      { Int $1 }
  | FLOAT
      { Float $1 }

  | OBRACE object_content CBRACE
      { Object $2 }
  | OBRACE CBRACE
      { Object (Hashtbl.create 0) }
  | OBRACK array_content CBRACK
      { Array $2 }
  | OBRACK CBRACK
      { Array [] }

  | TRUE
      { Bool true }
  | FALSE
      { Bool false }
  | NULL
      { Null }

parse:
    value EOF
      { $1 }
  | error EOF
      { Sparse.syntax_error 1 "syntax error parsing" }
