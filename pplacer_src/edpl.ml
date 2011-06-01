(* This is where we compute the EDPL distance. *)

open MapsSets

let of_pquery criterion rdist_uptri pq =
  let d p1 p2 =
    Edge_rdist.find_pairwise_dist
      rdist_uptri
      p1.Placement.location p1.Placement.distal_bl
      p2.Placement.location p2.Placement.distal_bl
  in
  let rec aux accum = function
    | x :: l ->
        aux
          (List.fold_left
            (fun tot y ->
              tot +. (criterion x) *. (criterion y) *. (d x y))
            accum
            l)
          l
    | _ -> accum
  in
  2. *. (aux 0. pq.Pquery.place_list)


  (*
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
*)
