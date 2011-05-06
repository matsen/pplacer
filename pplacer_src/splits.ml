open MapsSets

module Lset = IntSet
module LsetFuns = IntSetFuns

module OrderedLset = struct
  type t = Lset.t
  let compare = Lset.compare
end

module PprLset = struct
  type t = Lset.t
  let ppr = LsetFuns.ppr
end

module Lsetset = Set.Make(OrderedLset)
module LsetsetFuns = SetFuns (OrderedLset) (PprLset)

type split = Lset.t * Lset.t

let sort_split p =
  let (a,b) = p in
  if Lset.cardinal a < Lset.cardinal b then p else (b,a)

let make_split a b = sort_split (a,b)

module OrderedSplit = struct
  type t = split
  let compare (a1, a2) (b1, b2) =
    match Lset.compare a1 b1 with
      | 0 -> Lset.compare a2 b2
      | x -> x
end

module PprSplit = struct
  type t = split
  let ppr ff (a, b) =
    Format.fprintf ff "@[(";
    LsetFuns.ppr ff a;
    Format.fprintf ff ",@ ";
    LsetFuns.ppr ff b;
    Format.fprintf ff ")@]";
end

module Sset = Set.Make (OrderedSplit)
module SsetFuns = SetFuns (OrderedSplit) (PprSplit)


(* If sigma does not split X, then `split_lset(sigma, X)` returns the lsetset
 containing only X. Otherwise say sigma = U|V, in which case it returns the
 lsetset consisting of X intersect U and X intersect V.
 *)
let split_lset split lset =
  LsetsetFuns.of_list (
    List.filter (fun s -> not (Lset.is_empty s))
      [
        Lset.inter (fst split) lset;
        Lset.inter (snd split) lset;
      ]
  )

(* `split_lsetset(sigma, A)` will give the union of all of the lsetsets
  obtained by applying sigma to each of the lsets in A.
*)
let split_lsetset split lsetset =
  Lsetset.fold
    (fun ls lss -> Lsetset.union lss (split_lset split ls))
    lsetset
    Lsetset.empty

(* multiple application of `split_lsetset`. Take union at end. *)
let sset_lsetset = Sset.fold split_lsetset

(* does this split cut a given lset? *)
let split_does_cut_lset split lset =
  1 <> Lsetset.cardinal (split_lset split lset)

(* does this split cut one of the lset in the lsetset? *)
let split_does_cut_lsetset split lsetset =
  Lsetset.cardinal lsetset <> Lsetset.cardinal (split_lsetset split lsetset)

(* get the subset of splits that actually cut the given lsetset *)
let select_sset_cutting_lsetset splits leafss =
  Sset.filter
    (fun split -> split_does_cut_lsetset split leafss)
    splits

let lset_of_tree =
  let rec aux accum = function
    | Stree.Leaf n -> Lset.add n accum
    | Stree.Node (_, subtree) -> List.fold_left aux accum subtree
  in
  aux Lset.empty

(* Collect all of the splits together into a set. *)
let sset_of_tree =
  let rec aux accum above = function
    | Stree.Leaf n -> Sset.add (make_split above (Lset.singleton n)) accum
    | Stree.Node (_, subtree) ->
      List.fold_left
        (fun accum tree ->
          let above =
            List.fold_left
              (fun lset tree' ->
                if tree' = tree then
                  lset
                else
                  Lset.union lset (lset_of_tree tree'))
              above
              subtree
          in
          let ret = aux accum above tree in
          if Lset.is_empty above then
            ret
          else
            Sset.add
              (make_split above (lset_of_tree tree))
              ret)
        accum
        subtree
  in
  aux Sset.empty Lset.empty


