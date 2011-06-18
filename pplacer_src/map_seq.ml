(* For calculating Maximum A Posteriori sequences for internal locations on the
 * tree. *)

open Fam_batteries
open MapsSets
open Tax_id

let flip f x y = f y x

type pos = Distal | Proximal

(* first we need to grab u1 and u2. These can be some snodes.(0) and snodes.(1)
 * (fail nicely if these are out of bounds, which means that the ref tree has <
 * 1 leaf. *)

let of_map u1 u2 model t ~darr ~parr m cutoff =
  let bounded_max_index vec =
    let idx = Gsl_vector.max_index vec in
    if vec.{idx} /. (Linear_utils.l1_norm vec) < cutoff then -1 else idx
  and code = Model.code model in
    IntMap.mapi
    (fun id _ ->
      Model.to_sym_str
        code
        (Mutpick.get_summary
           Mutpick.Proximal
           bounded_max_index
           (-1) u1 u2 model t
           ~darr ~parr id))
    m

(* apply this one to the mrcam ... *)

let mrca_seq_map locmap mrcam tree =
  let rec aux accum = function
    | [] -> accum
    | (top_mrca, tree) :: rest ->
      let i = Stree.top_id tree in
      let accum' = match top_mrca with
        | Some top_mrca ->
          List.fold_left
            (flip (IntMap.add_listly top_mrca))
            accum
            (IntMap.get i [] locmap)
        | None -> accum
      and top_mrca' = if IntMap.mem i mrcam then Some i else top_mrca in
      let rest' = match tree with
        | Stree.Node (_, subtrees) ->
          List.fold_left
            (fun accum subtree -> (top_mrca', subtree) :: accum)
            rest
            subtrees
        | Stree.Leaf _ -> rest
      in
      aux accum' rest'
  in
  aux IntMap.empty [None, tree]

