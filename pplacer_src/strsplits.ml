(* Strsplits are string splits... the strings being names. *)

open Splits

module StrSet = MapsSets.StringSet
module StrSetFuns = MapsSets.StringSetFuns

module OrderedStrSet = struct
  type t = StrSet.t
  let compare = StrSet.compare
end

module PprStrSet = struct
  type t = StrSet.t
  let ppr = StrSetFuns.ppr
end

module SstrSet = Set.Make(OrderedStrSet)
module SstrSetFuns = MapsSets.SetFuns (OrderedStrSet) (PprStrSet)

let strset_of_gtree_and_lset gtree leafs =
  Lset.fold
    (fun n -> StrSet.add (Gtree.get_name gtree n))
    leafs
    StrSet.empty

type strsplit = StrSet.t * StrSet.t

let sort_strsplit p =
  let (a,b) = p in
  if StrSet.cardinal a < StrSet.cardinal b then p else (b,a)

let make_strsplit a b = sort_strsplit (a,b)

let strsplit_of_gtree_and_split gtree (a, b) =
  let of_lset = strset_of_gtree_and_lset gtree in
  of_lset a, of_lset b

module OrderedStrSplit = struct
  type t = strsplit
  let compare (a1, a2) (b1, b2) =
    match StrSet.compare a1 b1 with
      | 0 -> StrSet.compare a2 b2
      | x -> x
end

module PprStrSplit = struct
  type t = strsplit
  let ppr ff (a, b) =
    Format.fprintf ff "@[(";
    StrSetFuns.ppr ff a;
    Format.fprintf ff ",@ ";
    StrSetFuns.ppr ff b;
    Format.fprintf ff ")@]";
end

module StrSplitSet = Set.Make (OrderedStrSplit)
module StrSplitSetFuns = MapsSets.SetFuns (OrderedStrSplit) (PprStrSplit)

let strsplitset_of_gtree_and_splitset gtree splits =
  let of_split = strsplit_of_gtree_and_split gtree in
  Sset.fold
    (fun split -> StrSplitSet.add (of_split split))
    splits
    StrSplitSet.empty

let split_strset split strset =
  SstrSetFuns.of_list (
    List.filter (fun s -> not (StrSet.is_empty s))
      [
        StrSet.inter (fst split) strset;
        StrSet.inter (snd split) strset;
      ]
  )

let split_does_cut_strset split strset =
  1 <> SstrSet.cardinal (split_strset split strset)

let splits_intersect_strset splits strset =
  StrSplitSet.fold
    (fun (a, b) splits ->
      let a', b' = StrSet.inter a strset, StrSet.inter b strset in
      if StrSet.is_empty a' || StrSet.is_empty b' then
        splits
      else
        StrSplitSet.add (make_strsplit a' b') splits)
    splits
    StrSplitSet.empty
