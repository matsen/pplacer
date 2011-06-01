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

