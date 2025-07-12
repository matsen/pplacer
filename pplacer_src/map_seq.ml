(* For calculating Maximum A Posteriori sequences for internal locations on the
 * tree. For us, these internal locations will be mrcams. *)

open Ppatteries

(* Given
 * u1 and u2: utility Glvs
 * model t darr parr: as usual
 * m: map with internal node keys K
 * cutoff: posterior probability below which we don't mark base as known
 *
 * we make a map from each k in K to the MAP sequence for the Proximal side of
 * that internal node.
 * *)
let of_map (type a) (type b) m (u1: b) (u2: b) (model: a) t ~(darr: b array) ~(parr: b array) k_map cutoff =
  let module Model = (val m: Glvm.Model with type t = a and type glv_t = b) in
  let module Seq_post = Seq_post.Make(Model) in
  let bounded_max_index vec =
    let idx = Gsl.Vector.max_index vec in
    if vec.{idx} /. (Linear_utils.l1_norm vec) < cutoff then -1 else idx
  and code = Model.seq_type model |> Glvm.code in
  IntMap.mapi
    (fun id _ ->
      Glvm.to_sym_str
        code
        (Seq_post.get_summary
           Seq_post.Proximal
           bounded_max_index
           (-1) u1 u2 model t
           ~darr ~parr id))
    k_map

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
      let accum' =
        List.fold_left
          (flip (IntMap.add_listly top_mrca))
          accum
          (IntMap.get i [] locmap)
      and top_mrca' = if IntMap.mem i mrcam then i else top_mrca in
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
  aux IntMap.empty [Stree.top_id tree, tree]
