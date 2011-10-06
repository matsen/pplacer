open Ppatteries

let of_pql pql =
  List.fold_left
    (fun accum pq ->
      let pq_edges = Pquery.place_list pq
        |> List.enum
        |> Enum.map Placement.location
        |> IntSet.of_enum
      in
      List.fold_left
        (fun (did_match, accum') (edges, pqs) ->
          if IntSet.is_disjoint pq_edges edges then
            did_match, (edges, pqs) :: accum'
          else
            true, (IntSet.union pq_edges edges, pq :: pqs) :: accum')
        (false, [])
        accum
      |> first (fun b -> if b then None else Some (pq_edges, [pq]))
      |> uncurry maybe_cons)
    []
    pql
