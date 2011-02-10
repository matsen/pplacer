let of_string s =
  let lexbuf = Lexing.from_string s in
  Jsonparse.parse Jsonlex.token lexbuf
