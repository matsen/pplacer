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
    let idx = Gsl_vector.max_index vec in
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

let all_mrcas mrcam parent_map loc =
  Enum.seq loc (flip IntMap.find parent_map) (flip IntMap.mem parent_map)
    |> Enum.filter (flip IntMap.mem mrcam)

(* map_seqs is a map from node number to MAP sequence. *)
let reclassify_pquery_by_rejection ?ref_align rp map_seqs =
  let mrcam = Refpkg.get_mrcam rp in
  let gt = Refpkg.get_ref_tree rp in
  let st = Gtree.get_stree gt in
  let top_id = Stree.top_id st in
  let parent_map = Stree.parent_map st in
  let all_mrcas = all_mrcas mrcam parent_map in
  let ref_seqs_by_name = Option.default (Refpkg.get_aln_fasta rp) ref_align
    |> Array.enum
    |> Hashtbl.of_enum
  in
  (* Map from leaf number to reference sequence *)
  let ref_seqs_by_leaf = Newick_gtree.leaf_label_map gt
    |> IntMap.map (Hashtbl.find ref_seqs_by_name)
  in
  (* mrca_divergence is a map from a node number i to the maximum divergence
   * between the MAP sequence at i and the reference sequences below i.*)
  let mrca_divergence = flip IntMap.mapi map_seqs (fun i seq ->
    let divergence = Alignment.identity seq |- fst |- (-.) 1. in
    Stree.find i st
      |> Stree.leaf_ids
      |> List.map (flip IntMap.find ref_seqs_by_leaf |- divergence)
      |> List.max)
  in
  fun pq ->
    let divergence = Alignment.identity (Pquery.seq pq) |- fst |- (-.) 1. in
    let reclass p =
      let open Placement in
      let classif' = all_mrcas p.location
        |> Enum.filter (fun mrca ->
          let divergence_cutoff = IntMap.find mrca mrca_divergence *. 2. in
          divergence (IntMap.find mrca map_seqs) < divergence_cutoff)
        |> Enum.get
        |> Option.default top_id
        |> flip IntMap.find mrcam
      in
      {p with classif = Some classif'}
    in
    Pquery.apply_to_place_list (List.map reclass) pq
