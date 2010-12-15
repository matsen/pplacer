(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open Fam_batteries
open MapsSets
open Placement

let use_pp = ref false
and weighted = ref true
and refpkg_path = ref ""
and write_classic = ref false
and write_sing = ref false
and write_tog = ref false
and write_loc = ref false
and max_edpl = ref 0.
and white_bg = ref false
and bogus_bl = ref 0.1
and min_fat_bl = ref 1e-2
and print_tree_info = ref false
and show_node_numbers = ref false
and xml = ref false
and unit_width = ref 0.
and total_width = ref 400.
and log_coeff = ref 0.
and out_dir = ref "."

let parse_args () =
  let files  = ref [] in
   let usage =
    "placeviz "^Version.version_revision^"\nplaceviz ex.place\n"
  and anon_arg arg =
    files := arg :: !files in
  let args = 
    [
      "-p", Arg.Set use_pp,
      "Use posterior probability for the weight.";
      "--unweighted", Arg.Clear weighted,
      "Treat every placement as a point mass concentrated on the highest-weight placement.";
      "-c", Arg.Set_string refpkg_path,
      "Reference package path";
      "--classic", Arg.Set write_classic,
      "Write the fat and num trees in their individual files.";
      "--sing", Arg.Set write_sing,
      "Single placement: make one tree for each placement.";
      "--tog", Arg.Set write_tog,
      "Together placement: make a tree with each of the fragments represented as a pendant edge. Not recommended for more than 1000 placements.";
      "--loc", Arg.Set write_loc,
      "Write a fasta file sorted by location.";
      "--edpl", Arg.Set_float max_edpl,
      "Write out an EDPL tree such that the specified EDPL is the maximum.";
      "--whitebg", Arg.Set white_bg,
      "Make colors appropriate for a white background.";
      "--numBl", Arg.Set_float bogus_bl,
      "Set the branch length for visualization in the number tree.";
      "--min-fat", Arg.Set_float min_fat_bl,
      "The minimum branch length for fattened edges (to increase their visibility). To turn off set to 0.";
      "--nodeNumbers", Arg.Set show_node_numbers,
      "Put the node numbers in where the bootstraps usually go.";
      "--xml", Arg.Set xml,
      "Write phyloXML (with colors) for all visualizations.";
      "--total-width", Arg.Set_float total_width,
      "Set the total number of pixels for all of the mass (Default is 400).";
      "--unit-width", Arg.Set_float unit_width,
      "Set the number of pixels for a single placement (will override total-width if set).";
      "--log", Arg.Set_float log_coeff,
      "Set to a nonzero value to perform a logarithmic transform of the branch width.";
      "--outDir", Arg.Set_string out_dir,
      "Specify the directory to write place files to.";
  ] in
  Arg.parse args anon_arg usage;
  List.rev !files

     
    (* note return code of 0 is OK *)
let () =
  if not !Sys.interactive then begin
    let files = parse_args () in if files = [] then exit 0;
    let weighting = 
      if !weighted then Mass_map.Weighted
      else Mass_map.Unweighted
    and criterion = 
      if !use_pp then Placement.post_prob
      else Placement.ml_ratio
    in
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
        let ref_tree = Placerun.get_ref_tree placerun in
        let decor_ref_tree = 
          Decor_gtree.of_newick_gtree 
            (if not !show_node_numbers then ref_tree
            else (Newick.make_boot_id ref_tree))
        in
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
        let fname_base = 
          (!out_dir)^"/"^
            (Filename.basename 
              (Placerun_io.chop_place_extension fname))
        (* set up the coefficient for the width *)
        and n_placed = 
          (List.length pqueries) - (List.length unplaced_seqs) 
        and min_bl = match !min_fat_bl with 0. -> None | x -> Some x 
        in
        (* mass width is the amount of mass per unit *)
        let mass_width = 
          if !unit_width <> 0. then (* unit width specified *)
            !unit_width *. (float_of_int n_placed)
          else (* split up the mass according to the number of queries *)
            !total_width 
        in
        (* write loc file *)
        if !write_loc then
          Placeviz_core.write_loc_file 
            fname_base unplaced_seqs placed_map;
        (* make the various visualizations *)
        let place_massm = 
          Mass_map.By_edge.of_placerun weighting criterion placerun in
        if !write_classic then begin
          write_num_file fname_base decor_ref_tree placed_map;
          Placeviz_core.write_fat_tree ?min_bl
            mass_width !log_coeff fname_base decor_ref_tree place_massm
        end;
        if !write_tog then
          Placeviz_core.write_tog_file 
            tree_fmt criterion fname_base decor_ref_tree placed_map;
        if !max_edpl <> 0. then
          Placeviz_core.write_edpl_tree !white_bg weighting 
            criterion ~mass_width !log_coeff !max_edpl fname_base decor_ref_tree placerun;
        if !write_sing then
          Placeviz_core.write_sing_file 
            weighting
            criterion
            !unit_width
            tree_fmt
            fname_base 
            decor_ref_tree 
            (List.filter Pquery.is_placed pqueries);

        (* new version code *)
        let prname = 
          (Placerun.get_name placerun)^
            (if !use_pp then ".PP" else ".ML")
        and my_fat = Placeviz_core.fat_tree ?min_bl mass_width !log_coeff 
        in
        let (tax_rp_opt, final_rt) = 
          match Refpkg.refpkgo_of_path !refpkg_path with
          | None -> (None, decor_ref_tree)
          | Some rp -> 
              if Refpkg.tax_equipped rp then (Some rp, Refpkg.get_tax_ref_tree rp)
              else (None, decor_ref_tree)
        in
        Phyloxml.named_tree_list_to_file
          ([
            Some (prname^".ref.fat"),
            my_fat final_rt place_massm;
           ]
          @
          (try
            match tax_rp_opt with
            | None -> []
            | Some rp -> begin
              let (taxt, ti_imap) = Tax_gtree.of_refpkg_unit rp in
              [
                Some (prname^".tax.fat"),
                Placeviz_core.fat_tree (mass_width /. 2.) !log_coeff taxt
                (* NOTE: we don't use my_fat taxt here *)
                  (Mass_map.By_edge.of_pre
                    (Tax_mass.pre (Gtree.top_id taxt) Placement.contain_classif 
                      weighting criterion ti_imap placerun))
              ]
              end
          with
          | Placement.No_classif -> []))
          (fname_base^".xml");

        if frc = 0 && ret_code = 1 then 0 else ret_code
      with 
      | Sys_error msg -> prerr_endline msg; 2 
      | Placement.No_PP -> 
          failwith "Posterior probability use requested, but some or all placements were calculated without PP."
    in
    exit (List.fold_left collect 1 files)
  end
