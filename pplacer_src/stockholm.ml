type sline =
  | Header
  | Footer
  | Markup of string
  | Alignment of string * string

let stockholm_regexp = Str.regexp begin
  (* each line is either ... *)
  "^\\(" ^ (String.concat "\\|" [
    (* nothing but whitespace (matched at the end of the group), *)
    "";
    (* the header (group 2), *)
    "\\(# STOCKHOLM 1.0\\)";
    (* the footer (group 3), *)
    "\\(//\\)";
    (* markup (group 4), *)
    "\\(#=.+\\)";
    (* or a sequence alignment (group 5, containing groups 6 and 7). *)
    "\\(\\([^ \t\n#]+\\)[ \t]+\\([^ \t\n#]+\\)\\)";
  (* and finally, strip off any trailing whitespace. *)
  ]) ^ "\\)[ \t]*$\n*"
end

let rec first_match groups s =
  match groups with
    | g :: rest ->
      begin
        try
          g, Str.matched_group g s
        with
          | Not_found -> first_match rest s
      end
    | [] -> raise Not_found

let sline_of_match s =
  match first_match [2; 3; 4; 5] s with
    | 2, _ -> Header
    | 3, _ -> Footer
    | 4, s -> Markup s
    | 5, _ -> Alignment (Str.matched_group 6 s, Str.matched_group 7 s)
    | _, _ -> invalid_arg "sline_of_match"

exception Syntax_error of int * int
exception Parse_error of string

let pos s ch =
  let rec aux pos line =
    try
      let pos' = String.index_from s pos '\n' in
      if pos' >= ch then
        line, ch - pos
      else
        aux (succ pos') (succ line)
    with
      | Not_found -> line, ch - pos
  in aux 0 1

let tokenize_string s =
  let rec aux en accum =
    if String.length s = en then
      accum
    else if Str.string_match stockholm_regexp s en then
      let accum =
        try
          (sline_of_match s) :: accum
        with
          | Not_found -> accum
      in aux (Str.match_end ()) accum
    else
      let line, col = pos s en in
      raise (Syntax_error (line, col))
  in List.rev (aux 0 [])

type 'a phase =
  | Needs_header
  | Needs_footer of 'a list
  | Found_footer of 'a list

let parse tokens =
  let res = List.fold_left
    (fun state tok ->
      match state, tok with
        | Needs_header, Header -> Needs_footer []
        | Needs_header, _ -> raise (Parse_error "something before header")
        | _, Header -> raise (Parse_error "header unexpected")

        | Needs_footer l, Alignment (a, b) -> Needs_footer ((a, b) :: l)
        | Needs_footer l, Footer -> Found_footer l
        | Needs_footer l, _ -> Needs_footer l
        | _, Footer -> raise (Parse_error "footer unexpected")

        | Found_footer _, _ -> raise (Parse_error "something after footer")
    )
    Needs_header
    tokens
  in match res with
    | Found_footer l -> l
    | _ -> raise (Parse_error "didn't reach footer by EOF")

let of_string s =
  parse (tokenize_string s)

let of_file fname =
  let lines = File_parsing.string_list_of_file fname in
  let tokens = List.flatten (List.map tokenize_string lines) in
  parse tokens
