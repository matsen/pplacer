open Ppatteries
open Subcommand
open Guppy_cmdobjs

module I = Mass_map.Indiv

class cmd () =
object (self)
  inherit subcommand () as super
  inherit voronoi_cmd () as super_voronoi

  val query_seqs = flag "--query-seqs"
    (Plain ([], "A comma-separated list of leaves to turn into query sequences."))

  method specl =
    super_voronoi#specl
    @ [delimited_list_flag query_seqs]

  method desc = "finds a good collection of sequences to cut from a tree"
  method usage = "usage: min_adcl_tree [options] newick.tre"

  method action = function
    | [tree] ->
      let gt = Newick_gtree.of_file tree |> Newick_gtree.add_zero_root_bl
      and queries = fv query_seqs |> StringSet.of_list in
      let should_prune = Newick_gtree.leaf_label_map gt
        |> IntMap.enum
        |> Enum.filter_map
            (function k, v when StringSet.mem v queries -> Some k | _ -> None)
        |> IntSet.of_enum
        |> flip Stree.nodes_containing (Gtree.get_stree gt)
        |> flip IntSet.mem
      in
      let gt', pql = Newick_gtree.prune_to_pql should_prune gt in
      let mass_cb =
        if not (StringSet.is_empty queries) then
          Mass_map.Pre.of_pquery_list Mass_map.Spread Placement.ml_ratio pql
            |> I.of_pre
            |> const
        else fun diagram ->
          let mass = IntSet.cardinal diagram.Voronoi.all_leaves
            |> float_of_int
            |> (/.) 1.
          in
          IntSet.fold
            (flip IntMap.add_listly {I.mass; I.distal_bl = 0.0})
            diagram.Voronoi.all_leaves
            IntMap.empty
      in
      self#perform_voronoi gt' mass_cb

    | l ->
      List.length l
      |> Printf.sprintf "min_adcl_tree takes exactly one tree (%d given)"
      |> failwith

end
