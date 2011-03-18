open MapsSets


(* *** classification *** *)
let add_classif what how p = what p (how p)

let classify_pq what how pq =
  { pq with Pquery.place_list =
    List.map (add_classif what how) pq.Pquery.place_list }

let classify_pr what how pr =
  Placerun.set_pqueries pr
    (List.map (classify_pq what how) (Placerun.get_pqueries pr))

(* classification types *)
let classify_loc mrcam utm loc =
  let rec aux i =
    if IntMap.mem i mrcam then IntMap.find i mrcam
    else aux (IntMap.find i utm)
  in
  try aux loc with | Not_found -> Tax_id.NoTax

let classify mrcam utm p =
  classify_loc mrcam utm (Placement.location p)


