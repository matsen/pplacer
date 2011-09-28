open Ppatteries

let batchfile_regexp = Str.regexp begin
  String.concat "\\|" [
    (* whitespace (ignored) *)
    "[ \t]+";
    (* a newline (group 1) *)
    "\\(\r\\|\n\\|\r\n\\)";
    (* a bare string (group 2) *)
    "\\([^\" \t\r\n#]+\\)";
    (* a quoted string (group 3; group 4 should be non-matching) *)
    "\"\\(\\(\"\"\\|[^\"]\\)*\\)\"";
    (* comments (ignored) *)
    "#.*";
  ]
end

type token =
  | String of string
  | Newline
  | EOF

let token_of_match s =
  match Sparse.first_match [1; 2; 3] s with
    | 1, _ -> Newline
    | 2, s
    | 3, s -> String s
    | _, _ -> invalid_arg "token_of_match"

let tokenize_batchfile = Sparse.tokenize_string
  batchfile_regexp
  token_of_match
  ~eof_token:EOF

let quote_regexp = Str.regexp "\"\""
let parse tokens =
  let _, sll = Enum.fold
    (fun (sl, sll) -> function
      | String s ->
        let s = Str.global_replace quote_regexp "\"" s
        in s :: sl, sll
      | Newline
      | EOF ->
        if sl = [] then
          sl, sll
        else
          [], (List.rev sl) :: sll)
    ([], [])
    tokens
  in List.rev sll

let of_string, of_file = Sparse.gen_parsers tokenize_batchfile parse

let placeholder_regexp = Str.regexp begin
  String.concat "\\|" [
    (* an escaped brace (group 1) *)
    "\\({{\\|}}\\)";
    (* an identifier to substitute (group 2) *)
    "{\\([a-zA-Z0-9_-]+\\)}";
  ]
end
let substitute_placeholders m s =
  let substitute s =
    match Sparse.first_match [1; 2] s with
    | 1, "{{" -> "{"
    | 1, "}}" -> "}"
    | 2, identifier -> begin
      try
        StringMap.find identifier m
      with Not_found ->
        failwith ("unspecified batchfile substitution: " ^ identifier)
    end
    | _, _ -> invalid_arg "substitute"
  in
  Str.global_substitute
    placeholder_regexp
    substitute
    s

let argument_regexp = Str.regexp "^\\([a-zA-Z0-9_-]+\\)=\\(.*\\)$"
let split_arguments l =
  List.fold_left
    (fun accum s ->
      if not (Str.string_match argument_regexp s 0) then
        failwith ("malformed batchfile argument: " ^ s);
      StringMap.add
        (Str.matched_group 1 s)
        (Str.matched_group 2 s)
        accum)
    StringMap.empty
    l
