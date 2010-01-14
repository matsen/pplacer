(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * to calculate the errors of placement given a correct tree
 *
 * we assume for the purposes of this code that the tree is bifurcating, and is
 * rooted at a trifurcating node 
*)


open Fam_batteries
open MapsSets

let version_str = "v0.3"

let correct_tree_fname = ref ""

let parse_args () =
  let files  = ref [] in
  let usage =
    "errplace "^version_str^"\nerrplace -c correct_tree ex.place \
   calculates errors from the .place file based on the supplied correct tree.\
   We assume that the correct tree is bifurcating and rooted at a trifurcating \
   internal node."
  and anon_arg arg =
    files := arg :: !files in
  let args = [
    "-c", Arg.Set_string correct_tree_fname,
    "The correct tree."
  ] in
  Arg.parse args anon_arg usage;
  List.rev !files

  (* make a set of the taxon names *)
let tax_set t = 
  let rec get_name_list = function
    | id::l -> 
      begin
        match (Gtree.get_bark t id)#get_name_opt with
        | Some s -> s::(get_name_list l)
        | None -> get_name_list l
      end
    | [] -> []
  in
  StringSetFuns.of_list 
    (get_name_list (Gtree.collect_node_numbers t))
    
     
let () =
  if not !Sys.interactive then begin
    let files = parse_args () in if files = [] then exit 0;
    if !correct_tree_fname = "" then failwith "please supply correct tree";
    let correct_tree = Newick.of_file !correct_tree_fname in
    let correct_set = tax_set correct_tree in
    let collect ret_code place_fname =
      try
        let frc = 0 in
        let pr = Placerun_io.of_file place_fname in
        let ref_tree = Placerun.get_ref_tree pr in
        let ref_tax = tax_set ref_tree in
        assert(StringSet.diff ref_tax correct_set = StringSet.empty);
        let correct_loc = 
          match StringSet.elements (StringSet.diff correct_set ref_tax) with
          | [] -> failwith "correct has no extras!"
          | [extra_name] -> Dist_stree.find_placement correct_tree extra_name
          | _ -> failwith "correct has too many extras!" in
        let out_ch = open_out ((Filename.chop_extension place_fname)^".err") in
        List.iter (
          fun pq ->
            Printf.fprintf out_ch ">%s\n" (Pquery.name pq);
            List.iter (
              fun place ->
                (* for each placement, the error, the ml_ratio, and the pp *)
                Printf.fprintf out_ch "%d\t%g\t%s\n" 
                  (Dist_stree.edge_node_distance 
                    (Gtree.get_stree ref_tree) 
                    correct_loc 
                    (Placement.location place))
                  (Placement.ml_ratio place)
                  (Placement.opt_to_str "%g" (Placement.post_prob_opt place))
            ) (Pquery.place_list pq)
          ) (Placerun.get_pqueries pr);
        close_out out_ch;
        if frc = 0 && ret_code = 1 then 0 else ret_code
      with 
      | Sys_error msg -> prerr_endline msg; 2 
    in
    exit (List.fold_left collect 1 files)
  end
