(* For calculating Maximum A Posteriori sequences for internal locations on the
 * tree. For us, these internal locations will be mrcams. *)

open Ppatteries
module TIM = Tax_id.TaxIdMap

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

(* max_divergence_map, given a reference alignment, tree, and MAP sequences,
 * returns a map from a node number i to the maximum divergence between the MAP
 * sequence at i and the reference sequences below i. *)
let max_divergence_map ref_align gt map_seqs =
  let st = Gtree.get_stree gt in
  let ref_seqs_by_name = ref_align
    |> Array.enum
    |> Hashtbl.of_enum
  in
  (* Map from leaf number to reference sequence *)
  let ref_seqs_by_leaf = Newick_gtree.leaf_label_map gt
    |> IntMap.map (Hashtbl.find ref_seqs_by_name)
  in
  flip IntMap.mapi map_seqs (fun i seq ->
    let divergence = Alignment.divergence seq in
    Stree.find i st
      |> Stree.leaf_ids
      |> List.map (flip IntMap.find ref_seqs_by_leaf |- divergence)
      |> List.max)

(* map_seqs is a map from node number to MAP sequence. *)
let reclassify_pquery_by_rejection ?ref_align divergence_multiplier rp map_seqs =
  let parent_map = Refpkg.get_uptree_map rp in
  let mrcam = Refpkg.get_mrcam rp in
  let all_mrcas = all_mrcas mrcam parent_map in
  let gt = Refpkg.get_ref_tree rp in
  let top_id = Gtree.top_id gt in
  let mrca_divergence = max_divergence_map
    (Option.default (Refpkg.get_aln_fasta rp) ref_align)
    gt
    map_seqs
  in
  fun pq ->
    let divergence = Alignment.divergence (Pquery.seq pq) in
    let reclass p =
      let open Placement in
      let classif' = all_mrcas p.location
        |> Enum.filter (fun mrca ->
          let divergence_cutoff = IntMap.find mrca mrca_divergence
            *. divergence_multiplier
          in
          divergence (IntMap.find mrca map_seqs) < divergence_cutoff)
        |> Enum.get
        |> Option.default top_id
        |> flip IntMap.find mrcam
      in
      {p with classif = Some classif'}
    in
    Pquery.apply_to_place_list (List.map reclass) pq

let add_map_divergence_ratio ?ref_align rp map_seqs =
  let mrcam = Refpkg.get_mrcam rp in
  let node_divergence = max_divergence_map
    (Option.default (Refpkg.get_aln_fasta rp) ref_align)
    (Refpkg.get_ref_tree rp)
    map_seqs
  |> flip IntMap.find
  in
  let mrca_divergences = IntMap.enum mrcam
    |> Enum.map
        (fun (i, ti) -> ti, (IntMap.find i map_seqs, node_divergence i))
    |> Enum.fold (uncurry TIM.add_listly |> flip) TIM.empty
  in
  fun pq ->
    let divergence = Alignment.divergence (Pquery.seq pq) in
    let open Placement in
    let reclass = function
      | {classif = Some ti} as p when TIM.mem ti mrca_divergences ->
        let (_, max_dv), seq_dv = TIM.find ti mrca_divergences
          |> List.enum
          |> Enum.map (identity &&& (fst |- divergence))
          |> Enum.arg_min snd
        in
        if max_dv =~ 0. then p
        else {p with map_divergence_ratio = Some (seq_dv /. max_dv)}
      | p -> p
    in
    Pquery.apply_to_place_list (List.map reclass) pq

let apply_cutoff rp mrca_class cutoff =
  let tax_map =
    if mrca_class then Refpkg.get_mrcam rp
    else Edge_painting.of_refpkg rp
  and parent_map = Refpkg.get_uptree_map rp in
  let taxa_above = all_mrcas tax_map parent_map in
  fun pq ->
    let open Placement in
    let reclass = function
      | {classif = Some ti; map_divergence_ratio = Some ratio} as p
        when ratio > cutoff ->
        begin match taxa_above p.location
          |> Enum.map (flip IntMap.find tax_map)
          |> Enum.filter ((<>) ti)
          |> Enum.get
        with
        | Some ti' -> {p with classif = Some ti'}
        | None -> p
        end
      | p -> p
    in
    Pquery.apply_to_place_list (List.map reclass) pq

