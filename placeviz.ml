(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open Fam_batteries
open MapsSets
open Placement

let singly = ref false
and bogus_bl = ref 0.1
and print_tree_info = ref false
and show_node_numbers = ref false
and xml = ref false

let parse_args () =
  let files  = ref [] in
  let singly_opt = "-s", Arg.Set singly,
   "Single placement: make one tree for each placement."
  and bogus_bl_opt = "--bogusBl", Arg.Set_float bogus_bl,
   "Set the branch length for the subtrees/taxa which are collected together \
   for visualization in the number and together trees."
  and show_node_numbers_opt = "--nodeNumbers", Arg.Set show_node_numbers,
   "Put the node numbers in where the bootstraps usually go."
  and xml_opt = "--xml", Arg.Set xml,
   "Write phyloXML with colors."
  in
  let usage =
    "placeviz "^Placerun_io.version_str^"\nplaceviz ex.place\n"
  and anon_arg arg =
    files := arg :: !files in
  let args = 
    [singly_opt; bogus_bl_opt; show_node_numbers_opt; xml_opt] in
  Arg.parse args anon_arg usage;
  List.rev !files
     
    (* note return code of 0 is OK *)
let () =
  if not !Sys.interactive then begin
    let files = parse_args () in if files = [] then exit 0;
    let tree_writer ch itree = 
      if !xml then 
        Phyloxml.write_ftree ch (Ftree.make itree Decor.empty_decor)
      else Itree_io.write_newick ch itree in
    let write_num_file = 
      Placeviz_core.write_num_file !bogus_bl tree_writer in
    let collect ret_code fname =
      try
        let frc = 0 in
        let placerun = 
          Placerun_io.parse_place_file fname in
        let pre_ref_tree = Placerun.get_ref_tree placerun in
        let ref_tree = 
          if !show_node_numbers then Itree.make_boot_node_num pre_ref_tree
          else pre_ref_tree in
        let pqueries = Placerun.get_pqueries placerun in
        let unplaced_seqs, placed_map = 
          Pquery.make_map_by_best_loc
            Placement.ml_ratio
            pqueries
        in
        if unplaced_seqs <> [] then begin
          print_endline "Found the following unplaced sequences:";
          List.iter 
            (fun pq -> print_endline (Pquery.name pq))
            unplaced_seqs;
        end;
        let fname_base = Placerun_io.chop_place_extension fname in
        (* write loc file *)
        Placeviz_core.write_loc_file 
          fname_base unplaced_seqs placed_map;
        (* make the various visualizations *)
        Placeviz_core.write_tog_file 
          tree_writer fname_base ref_tree placed_map;
        write_num_file fname_base ref_tree placed_map;
        if !singly then
          Placeviz_core.write_sing_file 
            tree_writer
            fname_base 
            ref_tree 
            (List.filter Pquery.is_placed pqueries);
        if frc = 0 && ret_code = 1 then 0 else ret_code
      with Sys_error msg -> prerr_endline msg; 2 in
    exit (List.fold_left collect 1 files)
  end
