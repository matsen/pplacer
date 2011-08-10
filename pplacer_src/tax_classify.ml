open Ppatteries


(* *** classification *** *)
let add_classif what how p = what p (how p)

let classify_pq what how pq =
  { pq with Pquery.place_list =
    List.map (add_classif what how) pq.Pquery.place_list }

let classify_pr what how pr =
  Placerun.set_pqueries pr
    (List.map (classify_pq what how) (Placerun.get_pqueries pr))

(* Classify a placement at a given location.
 * Simply bounce up the tree until we hit an MRCA.
 *)
let classify_loc mrcam utm loc =
  let rec aux i =
    if IntMap.mem i mrcam then IntMap.find i mrcam
    else aux (IntMap.find i utm)
  in
  (* Note below that we start by bumping up the map one level. Thus, if we are
   * just distal to the MRCA of a clade, then we hit the MRCA on the nose. If
   * the placement is just proximal of the MRCA, we go to the ancestor of the
   * MRCA.
   * *)
  try aux (IntMap.find loc utm) with | Not_found -> Tax_id.NoTax

let classify mrcam utm p =
  classify_loc mrcam utm (Placement.location p)


