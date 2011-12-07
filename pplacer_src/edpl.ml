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

let average fl =
  List.fsum fl /. float_of_int (List.length fl)

let map_of_placerun criterion pr =
  let dm = Placerun.get_ref_tree pr |> Edge_rdist.build_pairwise_dist in
  List.fold_left
    (fun accum pq ->
      IntMap.add_listly
        (Pquery.best_location criterion pq)
        (of_pquery criterion dm pq)
        accum)
    IntMap.empty
    (Placerun.get_pqueries pr)
  |> IntMap.map average
