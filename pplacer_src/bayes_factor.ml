open Ppatteries
module TaxIdMap = Tax_id.TaxIdMap
module IAMR = IntAlgMapR

let of_refpkg rp =
  let post_map = Edge_painting.of_refpkg rp
    |> IntMap.values
    |> TaxIdMap.histogram_of_enum
    |> TaxIdMap.map float_of_int
  and td = Refpkg.get_taxonomy rp in
  fun criterion pq ->
    let evidence_m = List.fold_left
      (fun accum p ->
        let ti = Placement.classif p in
        try
          IAMR.add_by
            (Tax_taxonomy.get_tax_rank td ti)
            (criterion p /. TaxIdMap.find ti post_map)
            accum
        with Not_found ->
          Printf.sprintf
            "tax_id %s is not represented on the painted tree. this suggests the \
             placefile was classified with an old version of the reference package"
            (Tax_id.to_string ti)
          |> failwith)
      IAMR.empty
      (Pquery.place_list pq)
    in
    let evidence_of i = IAMR.soft_find i evidence_m
    and prev_represented = ref None in
    Array.mapi
      (fun i rankname ->
        let ev = evidence_of i in
        rankname, ev,
        if ev =~ 0. then
          None
        else
          let prev = !prev_represented in
          prev_represented := Some i;
          Some (match prev with
            | None -> infinity
            | Some i' ->
              let ev' = evidence_of i' in
              if ev' =~ 0. then
                neg_infinity
              else
                log (ev /. ev')))
      td.Tax_taxonomy.rank_names
