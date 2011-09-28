(* Taxonomic routines for gtrees.
 *
 * The purpose of this module is to make a "mrcam". A mrcam can be thought of as
 * the map that generates the taxonomic annotations in pplacer. Thus, it is a
 * map from an internal node to the taxid that is the MRCA of the nodes distal
 * to that internal node.
 *
 * Thus the keys of a MRCA are the internal nodes that are MRCAs.
*)


open Ppatteries

(* Make a map of leaf taxonomic annotations from the leaf names on the tree and
 * a seqinfo map (sim). *)
let tips_map sim t =
  IntMap.fold
    (fun k newick_bark m ->
      match newick_bark#get_name_opt with
      | Some name -> IntMap.add k (Tax_seqinfo.tax_id_by_name sim name) m
      | None -> m)
    (Gtree.get_bark_map t)
    IntMap.empty

(* Propogate the taxonomic information up the tree according to common ancestry,
 * starting with a map that only has taxonomic information at the tips.
 * *)
let fill_out td t tips_map =
  let m = ref tips_map in
  let _ =
    Gtree.recur
      (fun id below_tax_ids ->
        let mrca = Tax_taxonomy.list_mrca td below_tax_ids in
        m := IntMap.add id mrca (!m);
        mrca)
      (fun id -> IntMap.find id tips_map)
      t
  in
  !m

(* Attach names to actual MRCAs in the tree given a map from the previous step.
 * *)
let mrcam_of_full_map t full_map =
  let m = ref IntMap.empty in
  let _ =
    Gtree.recur
      (fun id below ->
        let our_tax_id = IntMap.find id full_map in
        List.iter
          (fun (below_id, below_tax_id) ->
            if our_tax_id <> below_tax_id then
              (* Something below is not the same tax_id as us.
               * Thus it is an MRCA and we label it as such. *)
              m := IntMap.add below_id below_tax_id (!m))
          below;
        (id, our_tax_id))
      (fun id -> (id, IntMap.find id full_map))
      t
  and top_id = Gtree.top_id t in
  IntMap.add top_id (IntMap.find top_id full_map) !m

let mrcam_of_data sim td t =
  mrcam_of_full_map t (fill_out td t (tips_map sim t))
