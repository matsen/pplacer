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
        (fun (matches, accum') (edges, pqs) ->
          if IntSet.disjoint pq_edges edges then
            matches, (edges, pqs) :: accum'
          else
            Some (match matches with
              | None -> IntSet.union pq_edges edges, pq :: pqs
              | Some (prev_edges, prev_pqs) ->
                IntSet.union prev_edges edges, List.append prev_pqs pqs),
            accum')
        (None, [])
        accum
      |> first (Option.default (pq_edges, [pq]))
      |> uncurry List.cons)
    []
    pql
