(* taxonomic routines for gtrees.
*)

open Fam_batteries
open MapsSets

(* here we get a map of tip taxonomic annotations *)
let tips_map sim t =
  IntMap.fold
    (fun k newick_bark m ->
      match newick_bark#get_name_opt with
      | Some name -> IntMap.add k (Tax_seqinfo.tax_id_by_name sim name) m
      | None -> m)
    (Gtree.get_bark_map t)
    IntMap.empty

(* next step is to propogate the taxonomic information up the tree according to
 * common ancestry. *)
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

(* the next step is to attach names to actual MRCAs in the tree. *)
let mrcam_of_full_map t full_map =
  let m = ref IntMap.empty in
  let _ =
    Gtree.recur
      (fun id below ->
        let our_tax_id = IntMap.find id full_map in
        List.iter
          (fun (below_id, below_tax_id) ->
            if our_tax_id <> below_tax_id then
              (* something below is not the same tax_id as us. thus it is an
               * MRCA and we label it as such *)
              m := IntMap.add below_id below_tax_id (!m))
          below;
        (id, our_tax_id))
      (fun id -> (id, IntMap.find id full_map))
      t
  in
  !m

let mrcam_of_data sim td t =
  mrcam_of_full_map t (fill_out td t (tips_map sim t))
