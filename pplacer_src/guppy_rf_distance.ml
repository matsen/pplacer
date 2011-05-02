open Subcommand
open Guppy_cmdobjs

module SSS = Seqsplits.SeqSplitSet

class cmd () =
object (self)
  inherit subcommand () as super
  inherit outfile_cmd () as super_outfile

  method specl = super_outfile#specl

  method desc = "calculates RF distance of two trees"
  method usage = "usage: rf_distance <newick_trees>"

  method action = function
    | [tree1; tree2] ->
      let tree1 = Newick_gtree.of_file tree1
      and tree2 = Newick_gtree.of_file tree2 in
      let to_seqset tree =
        Seqsplits.seqset_of_gtree_and_lset
          tree
          (Splits.get_lset (Gtree.get_stree tree))
      in
      let seqs1, seqs2 = to_seqset tree1, to_seqset tree2 in
      let seqs = Seqsplits.SeqSet.inter seqs1 seqs2 in
      let to_ss_set tree =
        let ss = Splits.get_sset (Gtree.get_stree tree) in
        let sss = Seqsplits.seqsplitset_of_gtree_and_splitset tree ss in
        SSS.filter
          (fun split -> Seqsplits.split_does_cut_seqset split seqs)
          sss
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
