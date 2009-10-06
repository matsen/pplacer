(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

(* set_model_name_and_gamma:
 * for both of these we set the given mutable variables. the reason why we do it
 * in the funny way is to make it easy for command line options to override what
 * we find in the file.
 * *)

open Fam_batteries

exception Stats_parsing_error of string

let str_match rex s = Str.string_match rex s 0


(* ************ RAXML ************* *)
let parse_raxml_info lines prefs = 
  let partition_rex = Str.regexp "^Partition:"
  and subst_matrix_rex = Str.regexp "^Substitution Matrix: \\(.*\\)"
  and base_freqs_rex = Str.regexp "pi(A): \\([^ ]+\\) pi(C): \\([^ ]+\\) pi(G): \\([^ ]+\\) pi(T): \\([^ ]+\\)"
  and inference_rex = Str.regexp "^Inference\\[0\\]: .* alpha\\[0\\]: \\([^ ]*\\) \\(.*\\)"
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
        let freqs_opt = 
          if (Prefs.model_name prefs) = "GTR" then begin
            let (base_freqs_line,_) = 
              try
                File_parsing.find_beginning 
                  (str_match base_freqs_rex) rest 
              with
              | Not_found -> 
                  raise (Stats_parsing_error "couldn't find base freqs line for GTR model")
            in
            let process_freq i = 
              try 
                float_of_string (Str.matched_group i base_freqs_line)
              with
              | Invalid_argument s -> 
                  raise (Stats_parsing_error ("problem parsing freqs: "^s))
            in
            Some (Array.map process_freq [|1;2;3;4|])
          end
          else
            None
        in
        let (inference_line,_) = 
          try
            File_parsing.find_beginning 
              (str_match inference_rex) rest 
          with
          | Not_found -> 
              raise (Stats_parsing_error "couldn't find inference line")
        in
                (* raxml gamma always 4 categories *)
        prefs.Prefs.gamma_n_cat := 4;
        prefs.Prefs.gamma_alpha := 
          float_of_string (Str.matched_group 1 inference_line);
        let rate_info = Str.matched_group 2 inference_line in
        let rates_freqs = 
          if str_match rates_rex rate_info then begin
            let freqs = 
              match freqs_opt with
              | Some f -> f
              | None -> 
                  raise (Stats_parsing_error 
                    ("GTR is the only allowed nucleotide model"))
            in
            if Prefs.model_name prefs <> "GTR" then
              raise (Stats_parsing_error ("have rates but model is not GTR!"));
            Some
              (freqs,
                Array.of_list
                  (List.map
                    float_of_string
                    (Str.split 
                      (Str.regexp "[ ]")
                      (Str.matched_group 1 rate_info))))
          end
          else begin
            (* we have amino acids *)
            if Prefs.model_name prefs == "GTR" then
              raise (Stats_parsing_error "no rates for GTR!");
            None 
          end
        in
        rates_freqs
      with
      | Not_found -> raise (Stats_parsing_error "problem parsing ")
    end
  end



(* ************ PHYML ************* *)

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
  float_of_string 
    (String.sub s beginning_len ((String.length s) - beginning_len))

let parse_phyml_stats lines prefs = 
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
        prefs.Prefs.gamma_alpha := float_of_string (Str.matched_group 1 alpha_str);
    end
    | _ -> raise (Stats_parsing_error "not enough lines after gamma!")
    end;
  let split_to_arr_and_rest l to_pull = 
    let (a,b) = list_split l to_pull in 
    (Array.of_list a, b)
  in
  try 
    let (_,nuc_f_lines) = 
      File_parsing.find_beginning (str_match nuc_freqs_rex) after_g in
    (* if we get here then we did find some nucleotide freq info *)
    if Prefs.model_name prefs <> "GTR" then 
      raise (Stats_parsing_error "model must be GTR if we are using nucleotides");
    let (freq_strs, after_freq) = 
      split_to_arr_and_rest (List.tl nuc_f_lines) 4 in
    let freqs = 
      ArrayFuns.map2 
      assert_and_extract_float
      [|"  - f(A)= ";"  - f(C)= ";"  - f(G)= ";"  - f(T)= "|]
      freq_strs
    in
    let transitions = 
      ArrayFuns.map2 
      assert_and_extract_float
      [|"  A <-> C"; "  A <-> G"; "  A <-> T"; "  C <-> G"; "  C <-> T"; "  G <-> T"|]
      (fst (split_to_arr_and_rest (snd (list_split after_freq 3)) 6))
    in
    Some (freqs,transitions)
  with
  | Not_found -> 
    if Prefs.model_name prefs == "GTR" then 
      raise (Stats_parsing_error "GTR model specified but I couldn't parse the frequency and rate information.");
    None


(* ************ BOTH ************* *)

let rec rex_matches_a_line rex = function
  | hd :: tl -> 
      if str_match rex hd then true
      else rex_matches_a_line rex tl
  | [] -> false

let raxml_header_rex = Str.regexp "^You are using RAxML"
let phyml_header_rex = Str.regexp "[ \t]*---  PhyML v3"

let parse_stats prefs = 
  let lines = 
    File_parsing.string_list_of_file (Prefs.stats_fname prefs) in
  try
    if rex_matches_a_line raxml_header_rex lines then
      parse_raxml_info lines prefs
    else if rex_matches_a_line phyml_header_rex lines then
      parse_phyml_stats lines prefs
    else
      raise (Stats_parsing_error "is this a RAxML v7 info or PHYML v3 statistics file? The header didn't match.")
  with
  | Stats_parsing_error s ->
      invalid_arg (Printf.sprintf "Problem parsing info or stats file %s: %s" (Prefs.stats_fname prefs) s)
