open Ppatteries


(* *** classification *** *)
let add_classif what how p = what p (how p)

let classify_pr what how pr =
  Placerun.apply_to_each_placement (add_classif what how) pr

(* Classify a placement at a given location.
 * Simply bounce up the tree until we hit an MRCA.
 *)
let mrca_classify_loc mrcam utm loc =
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

let mrca_classify mrcam utm p =
  mrca_classify_loc mrcam utm (Placement.location p)

let paint_classify paintm p =
  try IntMap.find (Placement.location p) paintm with | Not_found -> Tax_id.NoTax

