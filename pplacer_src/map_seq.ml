(* For calculating Maximum A Posteriori sequences for internal locations on the
 * tree. For us, these internal locations will be mrcams. *)

open Fam_batteries
open MapsSets
open Tax_id

let flip f x y = f y x

(* Given
 * u1 and u2: utility Glvs
 * model t darr parr: as usual
 * m: map with internal node keys K
 * cutoff: posterior probability below which we don't mark base as known
 *
 * we make a map from each k in K to the MAP sequence for the Proximal side of
 * that internal node.
 * *)
let of_map u1 u2 model t ~darr ~parr m cutoff =
  let bounded_max_index vec =
    let idx = Gsl_vector.max_index vec in
    if vec.{idx} /. (Linear_utils.l1_norm vec) < cutoff then -1 else idx
  and code = Model.code model in
  IntMap.mapi
    (fun id _ ->
      Model.to_sym_str
        code
        (Seq_post.get_summary
           Seq_post.Proximal
           bounded_max_index
           (-1) u1 u2 model t
           ~darr ~parr id))
    m

(* Given a map of tree locations to a list of something, the MRCA map of the
 * tree, and the tree itself, build map of MRCA locations to the combined list
 * of things below that MRCA.
 * 'a list IntMap.t -> 'b IntMap.t -> stree -> 'a list IntMap.t
 *)
let mrca_map_seq_map locmap mrcam tree =
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
