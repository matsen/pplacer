(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open Fam_batteries
open MapsSets
open Placement

let write_sing = ref false
let write_tog = ref false
let write_loc = ref false
and bogus_bl = ref 0.1
and print_tree_info = ref false
and show_node_numbers = ref false
and xml = ref false
and total_width = ref 100.

let parse_args () =
  let files  = ref [] in
   let usage =
    "placeviz "^Placerun_io.version_str^"\nplaceviz ex.place\n"
  and anon_arg arg =
    files := arg :: !files in
  let args = 
    [
      "--sing", Arg.Set write_sing,
      "Single placement: make one tree for each placement.";
      "--tog", Arg.Set write_tog,
      "Together placement: make a tree with each of the fragments represented as a pendant edge. Not recommended for more than 1000 placements.";
      "--loc", Arg.Set write_loc,
      "Write a fasta file sorted by location.";
      "--bogusBl", Arg.Set_float bogus_bl,
      "Set the branch length for visualization in the number tree.";
      "--nodeNumbers", Arg.Set show_node_numbers,
      "Put the node numbers in where the bootstraps usually go.";
      "--xml", Arg.Set xml,
      "Write phyloXML with colors.";
      "--width", Arg.Set_float total_width,
      "Set the total width for the fat tree and the sing tree.";
  ] in
  Arg.parse args anon_arg usage;
  List.rev !files

let criterion = Placement.ml_ratio
let weighting = Mass_map.Weighted
     
    (* note return code of 0 is OK *)
let () =
  if not !Sys.interactive then begin
    let files = parse_args () in if files = [] then exit 0;
    let tree_fmt = 
      if !xml then Placeviz_core.Phyloxml 
      else Placeviz_core.Newick in
    let write_num_file = 
      Placeviz_core.write_num_file !bogus_bl tree_fmt in
    let collect ret_code fname =
      try
        let frc = 0 in
        let placerun = 
          Placerun_io.of_file fname in
        let decor_ref_tree = 
          Decor_gtree.of_newick_gtree 
            (Placerun.get_ref_tree placerun) in
        let pqueries = Placerun.get_pqueries placerun in
        let unplaced_seqs, placed_map = 
          Pquery.make_map_by_best_loc
            criterion
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
        if !write_loc then
          Placeviz_core.write_loc_file 
            fname_base unplaced_seqs placed_map;
        (* make the various visualizations *)
        write_num_file fname_base decor_ref_tree placed_map;
        Placeviz_core.write_fat_tree 
          weighting criterion !total_width fname_base placerun;
        if !write_tog then
          Placeviz_core.write_tog_file 
            tree_fmt fname_base decor_ref_tree placed_map;
        if !write_sing then
          Placeviz_core.write_sing_file 
            !total_width
            tree_fmt
            fname_base 
            decor_ref_tree 
            (List.filter Pquery.is_placed pqueries);
        if frc = 0 && ret_code = 1 then 0 else ret_code
      with Sys_error msg -> prerr_endline msg; 2 in
    exit (List.fold_left collect 1 files)
  end
