(* This is where we compute the EDPL distance.
 *
 * "Raw" EDPL distance is not normalized to tree length.
 *
 * should we normalize by diameter instead?
 *
*)

open MapsSets

(* calculate quadratic form of an uptri u and a vector v *)
let qform u v =
  assert(Uptri.get_dim u = Array.length v);
  let tot = ref 0. in
  Uptri.iterij
    (fun i j x -> tot := !tot +. 2. *. (v.(i) *. v.(j) *. x))
    u;
  !tot

let raw_edpl_of_placement_array criterion t pa =
  let d = Distance_mat.of_placement_array t pa in
  qform
    d
    (Base.arr_normalized_prob (Array.map criterion pa))

let raw_edpl_of_placement_list criterion t pl =
  raw_edpl_of_placement_array criterion t (Array.of_list pl)

let raw_edpl_of_pquery criterion t pq =
  raw_edpl_of_placement_list
    criterion t (Pquery.place_list pq)

let edpl_of_pquery criterion t pq =
  (raw_edpl_of_pquery criterion t pq) /. (Gtree.tree_length t)

(* weight the edpl list by the mass. will throw an out of bounds if the top
 * id is not the biggest id in the tree. *)
let weighted_edpl_map weighting criterion t pquery_list =
  let top_id = Gtree.top_id t in
  let mass_a = Array.make (1+top_id) 0.
  and edpl_a = Array.make (1+top_id) 0.
  and tree_len = Gtree.tree_length t
  and mass_per_pquery = 1. /. (float_of_int (List.length pquery_list))
  in
  List.iter
    (fun pq ->
      let b = (raw_edpl_of_pquery criterion t pq) /. tree_len in
      List.iter
        (fun mu ->
          let i = mu.Mass_map.Pre.loc
          and mass = mu.Mass_map.Pre.mass
          in
          mass_a.(i) <- mass_a.(i) +. mass;
          edpl_a.(i) <- edpl_a.(i) +. mass *. b)
        (Mass_map.Pre.mul_of_pquery weighting criterion
           mass_per_pquery pq))
    pquery_list;
  let rec make_map accu i =
    if i < 0 then accu
    else
      make_map
        (if mass_a.(i) <> 0. then
          IntMap.add i (mass_a.(i), edpl_a.(i) /. mass_a.(i)) accu
        else
          accu)
        (i-1)
  in
  make_map IntMap.empty top_id

let weighted_edpl_map_of_pr weighting criterion pr =
  weighted_edpl_map
    weighting
    criterion
    (Placerun.get_ref_tree pr)
    (Placerun.get_pqueries pr)
