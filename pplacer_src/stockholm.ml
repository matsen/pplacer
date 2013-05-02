open Ppatteries

type sline =
  | Header
  | Footer
  | Markup of string
  | Alignment of string * string

let stockholm_regexp = Str.regexp begin
  (* each line is either ... *)
  "\\(" ^ (String.concat "\\|" [
    (* nothing but whitespace (matched at the end of the group), *)
    "";
    (* the header (group 2), *)
    "\\(# STOCKHOLM 1.0\\)";
    (* the footer (group 3), *)
    "\\(//\\)";
    (* markup (group 4), *)
    "\\(#=.+\\)";
    (* or a sequence alignment (group 5, containing groups 6 and 7). *)
    "\\(\\([^ \t\n\r#]+\\)[ \t]+\\([^ \t\n\r#]+\\)\\)";
  (* and finally, strip off any trailing whitespace. For an explanation of the
   * hideous regexp here, see fasta.ml. *)
  ]) ^ "\\)[ \t]*\\(\\(\r\\|\n\\|\r\n\\)+\\|$\\)"
end

let sline_of_match s =
  match Sparse.first_match [2; 3; 4; 5] s with
    | 2, _ -> Header
    | 3, _ -> Footer
    | 4, s -> Markup s
    | 5, _ -> Alignment (Str.matched_group 6 s, Str.matched_group 7 s)
    | _, _ -> invalid_arg "sline_of_match"

let tokenize_stockholm = Sparse.tokenize_string stockholm_regexp sline_of_match

module SM = MapsSets.StringMap
type phase =
  | Needs_header
  | Needs_footer of string list SM.t
  | Found_footer of (string * string) list

let gap_regexp = Str.regexp "\\."
let parse tokens =
  let res = Enum.fold
    (fun state tok ->
      match state, tok with
        | Needs_header, Header -> Needs_footer SM.empty
        | Needs_header, _ -> Sparse.syntax_error "something before header"
        | _, Header -> Sparse.syntax_error "header unexpected"

        | Needs_footer m, Alignment (name, seq) ->
          let seq = Str.global_replace gap_regexp "-" seq in
          Needs_footer (SM.add_listly name seq m)
        | Needs_footer m, Footer ->
          let l =
            SM.fold
              (fun name seql l ->
                let seq = List.rev seql |> String.concat "" in
                if not (List.is_empty l) then begin
                  let _, prev_seq = List.hd l in
                  if String.length seq <> String.length prev_seq then
                    Printf.sprintf "%s (length %d) doesn't match the previous sequence lengths (%d)"
                      name
                      (String.length seq)
                      (String.length prev_seq)
                    |> Sparse.syntax_error
                end;
                (name, seq) :: l)
              m
              []
          in Found_footer l
        | Needs_footer m, _ -> Needs_footer m
        | _, Footer -> Sparse.syntax_error "footer unexpected"

        | Found_footer _, _ -> Sparse.syntax_error "something after footer"
    )
    Needs_header
    tokens
  in match res with
    | Found_footer l -> l
    | _ -> Sparse.syntax_error "didn't reach footer by EOF"

let of_string, of_file = Sparse.gen_parsers tokenize_stockholm parse
let of_refpkg_contents = Refpkg_parse.of_file_or_string of_file of_string
