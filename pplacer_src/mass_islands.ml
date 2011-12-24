open Ppatteries

(* Mass islands. From the associated github issue:

   Say that two pqueries overlap if there is an edge such that the two pqueries
   both put mass on that edge. Define the overlap graph to be the graph with
   nodes being pqueries and edges signifying overlap. Call the connected
   components of the overlap graph "islands".

   We would like to have an algorithm to efficiently infer the islands.

   I would propose that it would go as follows:

   * think of islands as pairs of (E, P), P is a set of pqueries that are in
   the same island, and E is the set of edges that have mass assigned to them
   in the island.

   * proceed recursively, with the island set starting as empty

   * when given a new pquery p, first get the set of edges F that have mass for
   that pquery

   * if F does not intersect any of the E's from the list of islands, add a new
   island (F, {p})

   * if it intersects some set of islands, merge them and add in p.

*)

(* Output is a list of IntSet.t, Pquery.t list pairs which represent (E, P) as
 * described above. *)
let of_pql ?(discard_below = 0.) criterion pql =
  List.fold_left
    (fun accum pq ->
      let pq_edges = Pquery.place_list pq
        |> List.enum
        |> Enum.filter_map
            (fun p ->
              if criterion p < discard_below then
                None
              else
                Some (Placement.location p))
        |> IntSet.of_enum
      in
      if IntSet.is_empty pq_edges then accum else (* ... *)
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
