(* pplacer v1.1. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

let demulti_pquery_list pql = 
  List.flatten
    (List.map 
      (fun pq ->
        List.map
          (fun name -> {pq with Pquery.namel = [name]})
          pq.Pquery.namel)
      pql)

let demulti_placerun out_name pr = 
  {pr with 
    Placerun.pqueries = 
      demulti_pquery_list pr.Placerun.pqueries;
    name = out_name;}

(* UI-related *)

module Prefs = struct
  type prefs = 
    {
      out_prefix: string ref;
    }

  let out_prefix p = !(p.out_prefix)

  let defaults () =
    {
      out_prefix = ref "";
    }

  let specl_of_prefs prefs = 
[
  "-o", Arg.Set_string prefs.out_prefix,
  "Set the prefix to write to. Required.";
]
end

let of_argl = function
  | [] -> print_endline "splits apart placements with multiplicity, effectively undoing a round procedure."
  | argl -> 
    let prefs = Prefs.defaults () in
    (* note-- the command below mutates prefs (so order important) *)
    let fnamel = 
      Subcommand.wrap_parse_argv
        argl
        (Prefs.specl_of_prefs prefs)
        "usage: demulti [options] placefile[s]"
    in
    let out_prefix = Prefs.out_prefix prefs in
    if out_prefix = "" then 
      invalid_arg "Please specify an output prefix with -o";
    List.iter
      (fun fname ->
        let pr = Placerun_io.of_file fname in
        let out_name = (out_prefix^(pr.Placerun.name)) in
        Placerun_io.to_file
          (String.concat " " ("placeutil"::argl))
          (out_name^".place")
          (demulti_placerun out_name pr))
      fnamel

