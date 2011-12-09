(* This is where we compute the EDPL distance. *)

open Ppatteries

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

(* Take a weighted average of the elements of a list, the elements of which are
 * of the form (x, weight). *)
let weighted_average l =
  List.fsum (List.map (fun (a, b) -> a *. b) l) /. List.fsum (List.map snd l)

let map_of_placerun criterion pr =
  let dm = Placerun.get_ref_tree pr |> Edge_rdist.build_pairwise_dist in
  List.fold_left
    (fun accum pq ->
      let edpl_val = of_pquery criterion dm pq in
      List.fold_left
        (fun accum p ->
          IntMap.add_listly (Placement.location p) (edpl_val, criterion p) accum)
        accum
        (Pquery.place_list pq))
    IntMap.empty
    (Placerun.get_pqueries pr)
  |> IntMap.map weighted_average
