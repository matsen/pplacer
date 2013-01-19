(* Edge painting.

 The algorithm progressively builds up a map from edges to taxids. Maintain a
 list of unpainted edges. Start at the lowest (most specific) rank.

 For every unpainted edge, consider the collection of taxids cut by that edge.
 Is there exactly one? If so, then assign the edge to that taxid. If not, move
 up a rank.

*)

open Ppatteries
open Convex
open Stree

(* Build a map of node numbers (on the refpkg's reference tree) to the inferred
 * tax_id for each node. Used for classifying things placed onto the reference
 * tree. *)
let of_refpkg: Refpkg.t -> Tax_id.t IntMap.t = fun rp ->
  let gt = Refpkg.get_ref_tree rp in
  let st = Gtree.get_stree gt in
  (* rankmap is an edge-number-indexed map of rank-indexed maps of cutsetims for
   * the corresponding edge number and rank. *)
  let rankmap = rank_tax_map_of_refpkg rp
    |> IntMap.map (flip (curry build_sizemim_and_cutsetim) st %> snd)
  in
  let highest_rank, _ = IntMap.max_binding rankmap in
  node_ids st
    |> List.enum
    |> Enum.map
        (fun i ->
          let rec aux rank =
            if not (IntMap.mem rank rankmap) then aux (rank - 1) else (* ... *)
            let cset = IntMap.find rank rankmap |> IntMap.find i in
            if ColorSet.cardinal cset = 1 then
              i, ColorSet.choose cset
            else if rank = 0 then
              i, Tax_id.NoTax
            else
              aux (rank - 1)
          in
          aux highest_rank)
    |> IntMap.of_enum
