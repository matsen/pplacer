open Subcommand
open Guppy_cmdobjs
open Ppatteries
open Convex

class cmd () =
object (self)
  inherit Rppr_infer.cmd () as super_infer

  method desc = "reclassify nonconvex sequences in a reference package"
  method usage = "usage: reclass [options] -c my.refpkg"

  method action _ =
    let rp = self#get_rp in
    let gt = Refpkg.get_ref_tree rp in
    let st = Gtree.get_stree gt
    and leaf_labels = Newick_gtree.leaf_label_map gt
    and td = Refpkg.get_taxonomy rp
    and seqinfo = Refpkg.get_seqinfom rp in
    let taxa_set colorm needle =
      IntMap.fold
        (fun i ti accum -> if ti = needle then IntSet.add i accum else accum)
        colorm
        IntSet.empty
    and pair_dist = Edge_rdist.build_pairwise_dist gt
      |> Edge_rdist.find_pairwise_dist
    and seq_nodes = IntMap.enum leaf_labels
      |> Enum.map swap
      |> StringMap.of_enum
    in
    let avg_taxdist_from colorm ti j =
      let ts = taxa_set colorm ti in
      IntSet.fold (fun i accum -> pair_dist i 0. j 0. +. accum) ts 0.
      /. (IntSet.cardinal ts |> float_of_int)
    and rank_tax_map = rank_tax_map_of_refpkg rp in
    let rank, colors = Enum.find
      (fun (_, colors) ->
        let _, cutsetim = build_sizemim_and_cutsetim (colors, st) in
        let max_bad, _ = badness cutsetim in
        max_bad > 0)
      (IntMap.backwards rank_tax_map)
    in
    Tax_taxonomy.get_rank_name td rank
      |> dprintf "reclassifying at %s\n";
    let phi, _ = solve (colors, st) in
    let not_cut = nodeset_of_phi_and_tree phi st in
    let notax_colors = Stree.leaf_ids st
      |> IntSet.of_list
      |> flip IntSet.diff not_cut
      |> flip (IntSet.fold (flip IntMap.add Tax_id.NoTax)) colors
    in
    let uncolored_colors = IntMap.filter ((<>) Tax_id.NoTax) notax_colors in
    let notax_sizemim, _ = build_sizemim_and_cutsetim (notax_colors, st) in
    let st' = Rppr_infer.prune_notax notax_sizemim st in
    let gt' = Gtree.set_stree gt st' in
    let results = Rppr_infer.place_on_rp self#prefs rp gt' in
    dprint "finished classifying\n";
    let alternate_map = alternate_colors (uncolored_colors, st)
    and orig_sizemim, _ = build_sizemim_and_cutsetim (colors, st) in
    let orig_sizem = IntMap.find (Stree.top_id st) orig_sizemim
    and notax_sizem = IntMap.find (Stree.top_id st) notax_sizemim in
    List.map
      (fun (ti, _, seq) ->
        let prev_ti = Tax_seqinfo.tax_id_by_node_label seqinfo seq
        and i = StringMap.find seq seq_nodes in
        let alternates = IntMap.find i alternate_map
          |> ColorSet.elements
          |> List.map (Tax_taxonomy.get_lineage td)
          |> List.flatten
          |> ColorSet.of_list
        and prev_count = ColorMap.get prev_ti 0 orig_sizem in
        [seq;
         Tax_taxonomy.get_tax_name td prev_ti;
         Tax_id.to_string prev_ti;
         Tax_taxonomy.get_tax_name td ti;
         Tax_id.to_string ti;
         ColorSet.mem ti alternates |> string_of_bool;
         avg_taxdist_from uncolored_colors prev_ti i |> Printf.sprintf "%g";
         avg_taxdist_from uncolored_colors ti i |> Printf.sprintf "%g";
         prev_count |> string_of_int;
         prev_count - ColorMap.get prev_ti 0 notax_sizem |> string_of_int;
        ])
      results
    |> List.cons
        ["seq_name"; "old_name"; "old_taxid"; "new_name"; "new_taxid";
         "makes_convex"; "old_avg_dist"; "new_avg_dist"; "n_with_old";
         "n_nonconvex"]
    |> self#write_ll_tab

end
