(*

We will be calculating the Bayes factors on taxonomic rank, and they
will guide us to an understanding of what taxonomic level is correct.
Let ranks be numbered such that increasing numbers mean more specific
taxonomic ranks (as in implementation.)

For each rank r we will calculate an evidence; this is P(D|M), where
in this case D is the posterior probabilities observed on the edges, and
M is the hypothesis that r is the correct rank for classification. We
will be marginalizing over the classifications themselves.

P(D|M) = \sum_{\theta \in \Theta} P(\theta | M) P(D | \theta, M)

Here, \theta is the actual classification, and \Theta is the set of
classifications at rank r.

P(\theta | M) is the prior given the rank. We will take it to be one
over the number of edges painted with classification \theta. The
probabilistically right thing to do would be to normalize this such that
the sum of the priors is one, but this would penalize more specific
classifications in cases where there were lots of choices. We can try it
out.

P(D | \theta, M) is the likelihood given the classification. In our
case, the term that matters is the sum of the posterior probabilities
for edges with classification theta.

So, for every placement and every rank, we want to report the evidence
for that rank, which will be the sum of the following over
classifications theta of that rank:

  [sum of the probability mass for all edges painted theta] /
  [number of edges painted theta]

 * *)


open Ppatteries
module TaxIdMap = Tax_id.TaxIdMap
module IAMR = IntAlgMapR

type t = (string * float * float option) array

(* Fill in the normally-sparse MRCA map so that every node in the tree maps to
 * the appopriate MRCA, instead of just the nodes where the MRCAs occur. *)
let all_mrcas rp =
  let mrcam = Refpkg.get_mrcam rp
  and utm = Refpkg.get_uptree_map rp in
  let rec update mrcam i =
    match IntMap.Exceptionless.find i mrcam with
      | Some x -> mrcam, x
      | None ->
        let mrcam', x = update mrcam (IntMap.find i utm) in
        IntMap.add i x mrcam', x
  in
  Refpkg.get_ref_tree rp
    |> Gtree.get_stree
    |> Stree.node_ids
    |> List.fold_left (update |-- fst) mrcam

(* From a reference package, pquery, and criterion, determine the evidence and
 * evidence ratio (like a Bayes factor) for each rank. The returned value is an
 * array of rank names, evidences, and Bayes factor values (if applicable).
 * Note that this actually takes (see fun below) rp mrca_class criterion pq.
 * *)
let of_refpkg rp mrca_class =
  (* A map from each tax_id in the MRCA map to the number of times that tax_id
   * labels an edge in the tree. *)
  let denom_map = (if mrca_class then all_mrcas else Edge_painting.of_refpkg) rp
    |> IntMap.values
    |> TaxIdMap.histogram_of_enum
    |> TaxIdMap.map float_of_int
  and td = Refpkg.get_taxonomy rp in
  fun criterion pq ->
    (* Build up the evidence map, which maps from ranks to the average amount of
     * mass per edge. *)
    let evidence_m = List.fold_left
      (fun accum p ->
        match Placement.classif_opt p with
          | None
          | Some Tax_id.NoTax -> accum
          | Some ti -> (* ... *)
        try
          IAMR.add_by
            (Tax_taxonomy.get_tax_rank td ti)
            (criterion p /. TaxIdMap.find ti denom_map)
            accum
        with Not_found ->
          Printf.sprintf
            "tax_id %s is not represented on the classification tree. this suggests the \
             placefile was classified with an old version of the reference package"
            (Tax_id.to_string ti)
          |> failwith)
      IAMR.empty
      (Pquery.place_list pq)
    in
    let evidence_of i = IAMR.soft_find i evidence_m
    and prev_represented = ref None in
    (* Now go down the evidence map, calculating the Bayes factor-ish
     * quanitities for neighboring pairs of occupied ranks. *)
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
