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
  (* and finally, strip off any trailing whitespace. *)
  ]) ^ "\\)[ \t]*\\(\r\\|\n\\|\r\n\\)+"
end

let sline_of_match s =
  match Base.first_match [2; 3; 4; 5] s with
    | 2, _ -> Header
    | 3, _ -> Footer
    | 4, s -> Markup s
    | 5, _ -> Alignment (Str.matched_group 6 s, Str.matched_group 7 s)
    | _, _ -> invalid_arg "sline_of_match"

let tokenize_stockholm = Base.tokenize_string stockholm_regexp sline_of_match

exception Parse_error of string

module SM = MapsSets.StringMap
type phase =
  | Needs_header
  | Needs_footer of string list SM.t
  | Found_footer of (string * string) list

let gap_regexp = Str.regexp "\\."
let parse tokens =
  let res = List.fold_left
    (fun state tok ->
      match state, tok with
        | Needs_header, Header -> Needs_footer SM.empty
        | Needs_header, _ -> raise (Parse_error "something before header")
        | _, Header -> raise (Parse_error "header unexpected")

        | Needs_footer m, Alignment (name, seq) ->
          let seq = Str.global_replace gap_regexp "-" seq in
          let l =
            try
              SM.find name m
            with
              | Not_found -> []
          in
          let m = SM.add name (seq :: l) m in
          Needs_footer m
        | Needs_footer m, Footer ->
          let l =
            SM.fold
              (fun name seql l ->
                (name, String.concat "" (List.rev seql)) :: l)
              m
              []
          in Found_footer l
        | Needs_footer m, _ -> Needs_footer m
        | _, Footer -> raise (Parse_error "footer unexpected")

        | Found_footer _, _ -> raise (Parse_error "something after footer")
    )
    Needs_header
    tokens
  in match res with
    | Found_footer l -> l
    | _ -> raise (Parse_error "didn't reach footer by EOF")

let of_string s =
  parse (tokenize_stockholm s)

let of_file fname =
  let lines = File_parsing.string_list_of_file fname in
  let tokens = List.flatten (List.map tokenize_stockholm lines) in
  parse tokens
