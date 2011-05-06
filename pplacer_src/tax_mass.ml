open MapsSets
open Tax_id

exception Tax_id_not_in_tree of tax_id

let hashtbl_find_zero h k = if Hashtbl.mem h k then Hashtbl.find h k else 0.

let reverse_ti_imap start =
  IntMap.fold (fun k v -> TaxIdMap.check_add v k) start TaxIdMap.empty

(* here we build the pre mass map which is appropriate for the tax_gtree.
 * ti_imap takes us from the locations on the tree to taxids *)
let pre root_id tax_id_of_place weighting criterion ti_imap pr =
  let revm = reverse_ti_imap ti_imap in
  Mass_map.Pre.normalize_mass
    Mass_map.no_transform
    (List.map
      (fun pq ->
        {Mass_map.Pre.multi = 1;
         mul = List.map
          (fun p ->
            let ti = tax_id_of_place p in
            try
              {
                Mass_map.Pre.loc =
                  (if ti=Tax_id.NoTax then root_id
                  else TaxIdMap.find ti revm);
                distal_bl = 0.;
                mass = criterion p;
              }
            with
            | Not_found -> raise (Tax_id_not_in_tree ti))
          (Mass_map.place_list_of_pquery weighting criterion pq)})
      (Placerun.get_pqueries pr))
