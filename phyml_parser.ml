(* pplacer v0.2. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)


(* set_model_name_and_gamma:
 * set the given mutable variables. the reason why we do it in the funny way is
 * to make it easy for command line options to override what we find in the
 * file.
 * *)

let set_model_name_and_gamma fname model_name gamma_n_cat gamma_alpha = 
  let model_rex = Str.regexp 
  ". Model of nucleotides substitution:[ \t]+\\([A-Z]+\\)"
  and gamma_rex = Str.regexp 
  ". Discrete gamma model:[ \t]*Yes"
  and gamma_n_cats_rex = Str.regexp 
  "  - Number of categories:[ \t]*\\([0-9]*\\)"
  and gamma_alpha_rex = Str.regexp 
  "  - Gamma shape parameter:[ \t]*\\([0-9\\.]*\\)"
  in
  let rec aux = function
    | line::rest -> begin
      if Str.string_match model_rex line 0 then begin
        model_name := Str.matched_group 1 line;
        aux rest
      end
      else if Str.string_match gamma_rex line 0 then begin
        match rest with 
        | cats_str::alpha_str::rest2 -> begin
          if Str.string_match gamma_n_cats_rex cats_str 0 then
            gamma_n_cat := int_of_string (Str.matched_group 1 cats_str);
          if Str.string_match gamma_alpha_rex alpha_str 0 then
            gamma_alpha := float_of_string (Str.matched_group 1 alpha_str);
          aux rest2
        end
        | _ -> failwith "poorly formatted stats file"
      end
      else
        aux rest;
    end
    | [] -> ()
  in
  aux (Common_base.stringListOfFile fname)

let diagd_and_statd_of_phyml_file fname = 
  let (b, d) = NucModels.parseNucModel (Common_base.stringListOfFile fname) in
  (Diagd.normalizedOfExchangeableMat b d, d)
