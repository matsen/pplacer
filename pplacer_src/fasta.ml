type token =
  | Name of string
  | Sequence of string

let fasta_regexp = Str.regexp begin
  (* each line is either ... *)
  "\\(" ^ (String.concat "\\|" [
    (* nothing but whitespace (matched at the end of the group), *)
    "";
    (* a comment (not captured), *)
    ";[^\n\r]*";
    (* a sequence name (group 3; group 2 should be non-matching), *)
    "\\(>\\([^\n\r]+\\)\\)";
    (* or a sequence chunk (group 4). *)
    "\\([^\ \t\n\r]+\\)";
  (* and finally, strip off any trailing whitespace. *)
  ]) ^ "\\)[ \t]*\\(\r\\|\n\\|\r\n\\)+"
end

let token_of_match s =
  match Base.first_match [2; 4] s with
    | 2, _ -> Name (Str.matched_group 3 s)
    | 4, s -> Sequence s
    | _, _ -> invalid_arg "token_of_match"

let tokenize_fasta = Base.tokenize_string fasta_regexp token_of_match

exception Parse_error of string

type phase =
  | Beginning
  | Accumulating of (string * string) list * string * string list

let parse tokens =
  let combine res name seql = (name, String.concat "" (List.rev seql)) :: res in
  let res = List.fold_left
    (fun state tok ->
      match state, tok with
        | Beginning, Name n -> Accumulating ([], n, [])
        | Accumulating (res, name, seql), Sequence s ->
          Accumulating (res, name, s :: seql)
        | Accumulating (res, name, seql), Name n ->
          Accumulating (combine res name seql, n, [])

        | Beginning, Sequence _ -> raise (Parse_error "sequence before name")
    )
    Beginning
    tokens
  in match res with
    | Accumulating (res, name, seql) -> List.rev (combine res name seql)
    | Beginning -> []

let of_string s =
  parse (tokenize_fasta s)

let of_file fname =
  let lines = File_parsing.string_list_of_file fname in
  let tokens = List.flatten (List.map tokenize_fasta lines) in
  parse tokens
