(* Seqsplits are string splits... the strings being names. *)

open Splits

module SeqSet = MapsSets.StringSet
module SeqSetFuns = MapsSets.StringSetFuns

module OrderedSeqSet = struct
  type t = SeqSet.t
  let compare = SeqSet.compare
end

module PprSeqSet = struct
  type t = SeqSet.t
  let ppr = SeqSetFuns.ppr
end

module SseqSet = Set.Make(OrderedSeqSet)
module SseqSetFuns = MapsSets.SetFuns (OrderedSeqSet) (PprSeqSet)

let seqset_of_gtree_and_lset gtree leafs =
  Lset.fold
    (fun n -> SeqSet.add (Gtree.get_name gtree n))
    leafs
    SeqSet.empty

type seqsplit = SeqSet.t * SeqSet.t

let sort_seqsplit p =
  let (a,b) = p in
  if SeqSet.cardinal a < SeqSet.cardinal b then p else (b,a)

let make_seqsplit a b = sort_seqsplit (a,b)

let seqsplit_of_gtree_and_split gtree (a, b) =
  let of_lset = seqset_of_gtree_and_lset gtree in
  of_lset a, of_lset b

module OrderedSeqSplit = struct
  type t = seqsplit
  let compare (a1, a2) (b1, b2) =
    match SeqSet.compare a1 b1 with
      | 0 -> SeqSet.compare a2 b2
      | x -> x
end

module PprSeqSplit = struct
  type t = seqsplit
  let ppr ff (a, b) =
    Format.fprintf ff "@[(";
    SeqSetFuns.ppr ff a;
    Format.fprintf ff ",@ ";
    SeqSetFuns.ppr ff b;
    Format.fprintf ff ")@]";
end

module SeqSplitSet = Set.Make (OrderedSeqSplit)
module SeqSplitSetFuns = MapsSets.SetFuns (OrderedSeqSplit) (PprSeqSplit)

let seqsplitset_of_gtree_and_splitset gtree splits =
  let of_split = seqsplit_of_gtree_and_split gtree in
  Sset.fold
    (fun split -> SeqSplitSet.add (of_split split))
    splits
    SeqSplitSet.empty

let split_seqset split seqset =
  SseqSetFuns.of_list (
    List.filter (fun s -> not (SeqSet.is_empty s))
      [
        SeqSet.inter (fst split) seqset;
        SeqSet.inter (snd split) seqset;
      ]
  )

let split_does_cut_seqset split seqset =
  1 <> SseqSet.cardinal (split_seqset split seqset)

let splits_intersect_seqset splits seqset =
  SeqSplitSet.fold
    (fun (a, b) splits ->
      let a', b' = SeqSet.inter a seqset, SeqSet.inter b seqset in
      if SeqSet.is_empty a' || SeqSet.is_empty b' then
        splits
      else
        SeqSplitSet.add (make_seqsplit a' b') splits)
    splits
    SeqSplitSet.empty
