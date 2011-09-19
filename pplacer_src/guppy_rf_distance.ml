open Ppatteries
open Subcommand
open Guppy_cmdobjs

module SSS = Strsplits.StrSplitSet

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd () as super_outfile

  val rooted = flag "--rooted"
    (Plain (false, "Compare trees rooted-ly instead of unrooted-ly."))

  method specl = [
    toggle_flag rooted;
  ] @ super_outfile#specl

  method desc = "calculates RF distance of two trees"
  method usage = "usage: rf_distance <newick_trees>"

  method private add_virtual_root gt =
    let base_i = (Gtree.top_id gt) + 1 in
    let gt' = Gtree.add_bark
      base_i
      (new Newick_bark.newick_bark
         (`Of_bl_name_boot (None, Some "_virtual", None)))
      gt
    in
    {gt' with Gtree.stree =
        Stree.node
          (base_i + 1)
          [Stree.leaf base_i; Gtree.get_stree gt]}

  method private of_file name =
    let gt = Newick_gtree.of_file name in
    if fv rooted then
      self#add_virtual_root gt
    else
      gt

  method action = function
    | [tree1; tree2] ->
      let tree1 = self#of_file tree1
      and tree2 = self#of_file tree2 in
      let to_strset tree =
        Strsplits.strset_of_gtree_and_lset
          tree
          (Splits.lset_of_tree (Gtree.get_stree tree))
      in
      let strs1, strs2 = to_strset tree1, to_strset tree2 in
      let strs = Strsplits.StrSet.inter strs1 strs2 in
      let to_ss_set tree =
        let ss = Splits.sset_of_tree (Gtree.get_stree tree) in
        let sss = Strsplits.strsplitset_of_gtree_and_splitset tree ss in
        Strsplits.splits_intersect_strset sss strs
      in
      let sss1, sss2 = to_ss_set tree1, to_ss_set tree2 in
      let symmetric_diff = SSS.union (SSS.diff sss1 sss2) (SSS.diff sss2 sss1)
      in
      Printf.fprintf
        self#out_channel
        "%d\n"
        ((SSS.cardinal symmetric_diff) / 2)
    | _ -> ()
end
