(* parse statistics files from RAxML and Phyml
 *
 * Both of these parsing routines return a float array option. Protein models
 * correspond to a return value of None, and the array if it is given is an
 * array of transition frequencies.
 *)

open Ppatteries

exception Stats_parsing_error of string

let str_match rex s = Str.string_match rex s 0

let white_rex = Str.regexp "[ \n\t\r]*"

let remove_whitespace s = Str.global_replace white_rex "" s

let safe_float_of_string s =
  let no_white = remove_whitespace s in
  try float_of_string no_white with
 | Failure _ ->
     raise (Stats_parsing_error ("float_of_string failed on: '"^no_white^"'"))

let check_version program version known_versions =
  if not (List.mem version known_versions) then
    dprintf
      "WARNING: your stats file is from %s %s; %s has been tested with the following versions: %s\n"
      program
      version
      program
      (String.concat "; " known_versions)

let rec find_version_line rex = function
  | hd :: tl ->
      if str_match rex hd then Some (Str.matched_group 1 hd)
      else find_version_line rex tl
  | [] -> None

(*
# list_split [1;2;3;4;5] 2;;
- : int list * int list = ([1; 2], [3; 4; 5])
*)
let list_split l to_pull =
  let rec aux tp left = function
    | hd::tl as right ->
        if tp > 0 then aux (tp-1) (hd::left) tl
        else (List.rev left, right)
    | [] -> (List.rev left, [])
  in
  aux to_pull [] l

let assert_and_extract_float beginning s =
  let beginning_len = String.length beginning in
  for i=0 to beginning_len - 1 do
    if beginning.[i] <> s.[i] then
      raise (Stats_parsing_error "assert_and_extract_float: didn't match template!")
  done;
  safe_float_of_string
    (String.sub s beginning_len ((String.length s) - beginning_len))


(* ************ RAXML ************* *)

let raxml_header_rex = Str.regexp ".* RAxML version \\([^ ]+\\)"

let known_raxml_versions = [ "7.0.4"; "7.2.3"; "7.2.5"; "7.2.6"; "7.2.7"; ]

let parse_raxml_7_2_3_info lines prefs =
  let partition_rex = Str.regexp "^Partition:"
  and subst_matrix_rex = Str.regexp "^Substitution Matrix: \\(.*\\)"
  and alpha_rex = Str.regexp ".*alpha\\[0\\]: \\([^ ]*\\) \\(.*\\)"
  and rates_rex = Str.regexp "^rates\\[0\\] ac ag at cg ct gt: \\(.*\\)"
  in
  match
    (File_parsing.partition_list
      (str_match partition_rex)
      lines) with
  | [] | [_] -> raise (Stats_parsing_error "couldn't find a partition line")
  | _::partitions -> begin
    if List.length partitions > 1 then
      raise (Stats_parsing_error "too many partitions. Only one is allowed.")
    else begin
      try
        let (subs_line, rest) =
          try
            File_parsing.find_beginning
              (str_match subst_matrix_rex)
              (List.hd partitions)
          with
          | Not_found ->
              raise (Stats_parsing_error "couldn't find substitution matrix line")
        in
        prefs.Prefs.model_name := Str.matched_group 1 subs_line;
        let (alpha_line,_) =
          try
            File_parsing.find_beginning
              (str_match alpha_rex) rest
          with
          | Not_found ->
              raise (Stats_parsing_error "couldn't find alpha line")
        in
        (* raxml gamma always 4 categories *)
        let alpha_str = Str.matched_group 1 alpha_line
        and rate_info = Str.matched_group 2 alpha_line in
        prefs.Prefs.gamma_n_cat := 4;
        prefs.Prefs.gamma_alpha := safe_float_of_string alpha_str;
        if str_match rates_rex rate_info then begin
          if Prefs.model_name prefs <> "GTR" then
            raise (Stats_parsing_error ("have rates but model is not GTR! GTR is only allowed nucleotide model."));
          Some
            (Array.of_list
              (List.map
                safe_float_of_string
                (Str.split
                  (Str.regexp "[ ]")
                  (Str.matched_group 1 rate_info))))
        end
        else if Prefs.model_name prefs == "GTR" then
          raise (Stats_parsing_error "GTR model specified, but no rates found.")
        else
          None
      with
      | Not_found -> raise (Stats_parsing_error "problem parsing ")
      | Invalid_argument s -> raise (Stats_parsing_error ("problem parsing: "^s))
    end
  end

(* parse re-estimated RAxML info file *)
let parse_raxml_re_estimated_info lines prefs =
  let partition_rex = Str.regexp "^Partition:"
  and data_type_rex = Str.regexp "^DataType: \\(.*\\)"
  and subst_matrix_rex = Str.regexp "^Substitution Matrix: \\(.*\\)"
  and alpha_rex = Str.regexp "^alpha: \\(.*\\)"
  in
  match
    (File_parsing.partition_list
      (str_match partition_rex)
      lines) with
  | [] | [_] -> raise (Stats_parsing_error "couldn't find a partition line")
  | _::partitions -> begin
    if List.length partitions > 1 then
      raise (Stats_parsing_error "too many partitions. Only one is allowed.")
    else begin
      try
        let (data_type_line, rest) =
          try
            File_parsing.find_beginning
              (str_match data_type_rex)
              (List.hd partitions)
          with
          | Not_found ->
              raise (Stats_parsing_error "couldn't find data type line")
        in
        let data_type_str = Str.matched_group 1 data_type_line
        and () = match rest with
        | subst_matrix_line::_ ->
            if str_match subst_matrix_rex subst_matrix_line <> true then
              raise (Stats_parsing_error "couldn't match substitution matrix line");
            prefs.Prefs.model_name := Str.matched_group 1 subst_matrix_line;
        | [] -> raise (Stats_parsing_error "unexpected end of file after subs matrix line")
        in
        let (alpha_line, final_lines) =
          try
            File_parsing.find_beginning (str_match alpha_rex) rest
          with
          | Not_found ->
              raise (Stats_parsing_error "couldn't find alpha line")
        in
        (* raxml gamma always 4 categories *)
        prefs.Prefs.gamma_n_cat := 4;
        prefs.Prefs.gamma_alpha :=
          safe_float_of_string (Str.matched_group 1 alpha_line);
        match data_type_str with
        | "DNA" -> begin
            match final_lines with
            | _::rate_lines ->
              Some
                (Array.map2
                  assert_and_extract_float
                  [|
                    "rate A <-> C:";
                    "rate A <-> G:";
                    "rate A <-> T:";
                    "rate C <-> G:";
                    "rate C <-> T:";
                    "rate G <-> T:";
                  |]
                  (Array.of_list (fst (list_split rate_lines 6))))
            | [] -> raise (Stats_parsing_error "unexpected end of file after alpha line")
          end
        | "AA" -> None
        | s -> raise (Stats_parsing_error ("data type unknown: "^s))
      with
      | Not_found -> raise (Stats_parsing_error "problem parsing ")
    end
  end


let parse_raxml_info version lines prefs =
  (* -f e is re-estimate branch lengths, makes a different info file *)
  let invocation_line_rex = Str.regexp ".*raxmlHPC.*-f e \\(.*\\)" in
  check_version "RAxML" version known_raxml_versions;
  match version with
  | "7.0.4"
  | "7.2.3"
  | "7.2.5"
  | "7.2.6"
  | "7.2.7" -> begin
      match find_version_line invocation_line_rex lines with
        | Some _ -> begin
            dprint ("parsing a re-estimated RAxML info file.");
            parse_raxml_re_estimated_info lines prefs
        end
        | None -> parse_raxml_7_2_3_info lines prefs
      end
  | _ ->
      dprint "I'm going to try parsing as if this was version 7.2.3";
      parse_raxml_7_2_3_info lines prefs



(* ************ PHYML ************* *)

let phyml_header_rex = Str.regexp "[ \t]*---  PhyML v\\([^ ]+\\)"

let known_phyml_versions = [ "3.0"; "3.0_246M"; ]

let parse_phyml_stats version lines prefs =
  let model_rex = Str.regexp
  ". Model of .* substitution:[ \t]+\\([A-Z]+\\)"
  and gamma_rex = Str.regexp
  ". Discrete gamma model:[ \t]*\\(.*\\)"
  and gamma_n_cats_rex = Str.regexp
  "  - Number of categories:[ \t]*\\([0-9]*\\)"
  and gamma_alpha_rex = Str.regexp
  "  - Gamma shape parameter:[ \t]*\\([0-9\\.]*\\)"
  and nuc_freqs_rex = Str.regexp
  ". Nucleotides frequencies:"
  in
  check_version "phyml" version known_phyml_versions;
  let (m_line, after_m) =
    try
      File_parsing.find_beginning
        (str_match model_rex)
        lines
    with
    | Not_found ->
        raise (Stats_parsing_error "couldn't find model line!")
  in
  prefs.Prefs.model_name := Str.matched_group 1 m_line;
  let (g_line, after_g) =
    File_parsing.find_beginning (str_match gamma_rex) after_m in
  if "Yes" = Str.matched_group 1 g_line then begin
    match after_g with
    | cats_str::alpha_str::_ -> begin
      if Str.string_match gamma_n_cats_rex cats_str 0 then
        prefs.Prefs.gamma_n_cat := int_of_string (Str.matched_group 1 cats_str);
      if Str.string_match gamma_alpha_rex alpha_str 0 then
        prefs.Prefs.gamma_alpha := safe_float_of_string (Str.matched_group 1 alpha_str);
    end
    | _ -> raise (Stats_parsing_error "not enough lines after gamma!")
    end;
  try
    let (_,nuc_f_lines) =
      File_parsing.find_beginning (str_match nuc_freqs_rex) after_g in
    (* if we get here then we did find some nucleotide freq info *)
    if Prefs.model_name prefs <> "GTR" then
      raise (Stats_parsing_error "model must be GTR if we are using nucleotides");
    let (_, after_freq) = list_split (List.tl nuc_f_lines) 4 in
    Some
      (ArrayFuns.map2
      assert_and_extract_float
      [|"  A <-> C"; "  A <-> G"; "  A <-> T"; "  C <-> G"; "  C <-> T"; "  G <-> T"|]
      (Array.of_list (fst (list_split (snd (list_split after_freq 3)) 6))))

  with
  | Not_found ->
    if Prefs.model_name prefs == "GTR" then
      raise (Stats_parsing_error "GTR model specified but I couldn't parse the frequency and rate information.");
    None


(* JSON! *)

open Simple_json

exception Unknown_ras_model of string
exception Wrong_field_type of string
exception Unknown_datatype of string
exception Unsupported_model of string

let parse_json prefs path =
  let root = of_file path in
  let subs_model = find_string root "subs_model" in
  prefs.Prefs.model_name := subs_model;
  let () =
    match (find root "ras_model") with
    | Jsontype.Null -> ()
    | Jsontype.String "gamma" -> begin
        let gamma_obj = find root "gamma" in
        prefs.Prefs.gamma_n_cat := find_int gamma_obj "n_cats";
        prefs.Prefs.gamma_alpha := find_float gamma_obj "alpha";
      end
    | Jsontype.String ras_string -> raise (Unknown_ras_model ras_string)
    | _ -> raise (Wrong_field_type "ras_model")
  in
  match (find_string root "datatype") with
  | "DNA" -> begin
    match subs_model with
    | "GTR" as name -> begin
      prefs.Prefs.model_name := name;
      let subs_rates = find root "subs_rates" in
      Some (Array.map (find_float subs_rates) [|"ac";"ag";"at";"cg";"ct";"gt"|])
    end
    | s -> raise (Unsupported_model s)
  end
  | "AA" -> begin
    match subs_model with
    | "WAG" -> None
    | s -> begin
        Printf.printf "We don't currently support %s. If you want it incorporated, email Erick." s;
        raise (Unsupported_model s)
    end
  end
  | s -> raise (Unknown_datatype s)



(* ************ main fun ************* *)

let parse_stats ref_dir_complete prefs =
  let path = ref_dir_complete^(Prefs.stats_fname prefs) in
  if Filename.check_suffix path ".json" then parse_json prefs path
  else try
    let lines =
      File_parsing.string_list_of_file
        (ref_dir_complete^(Prefs.stats_fname prefs)) in
    match find_version_line raxml_header_rex lines with
    | Some v -> parse_raxml_info v lines prefs
    | None -> begin
      match find_version_line phyml_header_rex lines with
      | Some v -> parse_phyml_stats v lines prefs
      | None ->
          raise (Stats_parsing_error "is this a RAxML v7 info or PHYML v3 statistics file? The header didn't match.")
    end
  with
  | ex ->
      print_endline ("Problem parsing info or stats file"^(Prefs.stats_fname prefs));
      raise ex
