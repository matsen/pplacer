open Ppatteries

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
    "\\(>[ \t]*\\([^ \t\n\r]+\\)[^\n\r]*\\)";
    (* or a sequence chunk (group 4). *)
    "\\([^\ \t\n\r]+\\)";
  (* and finally, strip off any trailing whitespace. The last bit of this
   * deserves some explanation, as it's a little unreadable with the
   * backslashes. It matches successive sequences of \r, \n, or \r\n, /or/ the
   * end of the string. Just the latter isn't good enough because it won't work
   * with files delimited with just \r, and just the former isn't good enough
   * because it won't match on strings without linebreaks. *)
  ]) ^ "\\)[ \t]*\\(\\(\r\\|\n\\|\r\n\\)+\\|$\\)"
end

let token_of_match s =
  match Sparse.first_match [2; 4] s with
    | 2, _ -> Name (Str.matched_group 3 s)
    | 4, s -> Sequence s
    | _, _ -> invalid_arg "token_of_match"

let tokenize_fasta = Sparse.tokenize_string fasta_regexp token_of_match

type phase =
  | Beginning
  | Accumulating of (string * string) list * string * string list

let parse tokens =
  let combine res name seql = (name, String.concat "" (List.rev seql)) :: res in
  let res = Enum.fold
    (fun state tok ->
      match state, tok with
        | Beginning, Name n -> Accumulating ([], n, [])
        | Accumulating (res, name, seql), Sequence s ->
          Accumulating (res, name, s :: seql)
        | Accumulating (res, name, seql), Name n ->
          Accumulating (combine res name seql, n, [])

        | Beginning, Sequence _ -> Sparse.syntax_error "sequence before name")
    Beginning
    tokens
  in match res with
    | Accumulating (res, name, seql) -> List.rev (combine res name seql)
    | Beginning -> []

let of_string, of_file = Sparse.gen_parsers tokenize_fasta parse
let of_refpkg_contents = Refpkg_parse.of_file_or_string of_file of_string
