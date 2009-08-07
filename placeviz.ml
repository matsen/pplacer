(* pplacer v0.2. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open Fam_batteries
open MapsSets
open Placement

let version_str = "v0.2"

let singly = ref false
and ml_cutoff = ref 0.
and bogus_bl = ref 0.1
and print_tree_info = ref false
and show_node_numbers = ref false

let parse_args () =
  let files  = ref [] in
  let singly_opt = "-s", Arg.Set singly,
   "Single placement: make one tree for each placement."
  and ml_cutoff_opt = "-l", Arg.Set_float ml_cutoff,
   "The ML likelihood weight ratio cutoff for inclusion."
  and bogus_bl_opt = "--bogusBl", Arg.Set_float bogus_bl,
   "Set the branch length for the subtrees/taxa which are collected together \
   for visualization in the number and together trees."
  and tree_info_opt = "--treeInfo", Arg.Set print_tree_info,
   "Print out the information attached to each node of the tree."
  and show_node_numbers_opt = "--nodeNumbers", Arg.Set show_node_numbers,
   "Put the node numbers in where the bootstraps usually go."
  in
  let usage =
    "placeviz "^version_str^"\nplaceviz ex.place\n"
  and anon_arg arg =
    files := arg :: !files in
  let args = 
    [singly_opt; ml_cutoff_opt; bogus_bl_opt; tree_info_opt; show_node_numbers_opt] in
  Arg.parse args anon_arg usage;
  List.rev !files

     
    (* note return code of 0 is OK *)
let () =
  if not !Sys.interactive then begin
    let files = parse_args () in if files = [] then exit 0;
    let collect ret_code place_fname =
      try
        let frc = 0 in
        let fname_base = 
          (Filename.chop_extension place_fname)^(
            Printf.sprintf ".L%02d" (int_of_float (100. *. !ml_cutoff))) in
        let (pre_ref_tree, named_places) = 
          Placement_io.parse_place_file version_str place_fname in
        let ref_tree = 
          if !show_node_numbers then Stree.make_boot_node_num pre_ref_tree
          else pre_ref_tree in
        let best_place_hash = 
          Placement.id_best_hash_of_placement_list 
            Placement.compare_ml_place 
            (Placement.make_ml_ratio_filter !ml_cutoff) 
            named_places
        in
        (* print some statistics *)
        Printf.printf "in %s, %d of %d made it through filter\n"
          place_fname
          (List.length (HashtblFuns.keys best_place_hash))
          (List.length named_places);
        (* singly : one-read-at-a-time placement *)
        if !singly then begin
          let by_name_place_map = by_name_map_of_place_hash best_place_hash in
          let out_ch = open_out (fname_base^".sing.tre") in
          List.iter (
            fun (name, _) ->
              if StringMap.mem name by_name_place_map then begin
                (* this read gets placed *)
                let best_place = StringMap.find name by_name_place_map in
                Printf.fprintf out_ch "%s\n" (
                  Stree_io.to_newick (
                    Place_in_tree.place_single 
                    name best_place ref_tree))
              end
          ) named_places;
          close_out out_ch;
        end;
        if !print_tree_info then Stree_io.print_tree_info ref_tree;
        (* number placement : counting the number of reads on an edge *)
        let out_ch = open_out (fname_base^".num.tre") in
        Stree_io.write_newick out_ch (
          Place_in_tree.number_place !bogus_bl best_place_hash ref_tree
        );
        close_out out_ch;
        (* together placement : all reads for a single location in a clade *)
        let out_ch = open_out (fname_base^".tog.tre") in
        Stree_io.write_newick out_ch (
          Place_in_tree.together_place !bogus_bl best_place_hash ref_tree
        );
        close_out out_ch;
  
        if frc = 0 && ret_code = 1 then 0 else ret_code
      with Sys_error msg -> prerr_endline msg; 2 in
    exit (List.fold_left collect 1 files)
  end
