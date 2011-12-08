open Ppatteries

(* Compress pqueries. From the associated github issue:

   A cutoff c is specified via a command line flag. We will be merging pairs of
   pqueries that have KR distance between them less than c.

   * divide the pqueries into islands.

   * for each island, calculate all of the pairwise distances between the
   pqueries for that island and put them in a matrix

   * build a graph such that the nodes are pqueries, and there is an edge
   between them if their distance is less than c (note that this can be a
   binary matrix that is 1 if the distance is less than c and 0 otherwise, an
   easy component-wise application of a function to the pairwise distance
   matrix)

   * find a vertex cover for this graph. Although we could find a minimal
   vertex cover, I think it makes more sense to just use a simple greedy
   algorithm as described below.

   * merge pqueries according to this vertex cover as described below.

   We will need to put the pqueries in an equivalently-ordered array so that we
   can go from indices to actual pqueries. Because we will be wanting to go
   back and forth between the actual pqueries and entries in the matrix, I
   strongly suggest just using the indices instead of the actual pqueries
   below. I'll still call them pqueries or nodes for convenience.


   == Finding a vertex cover ==

   The goal is to find a set S of nodes such that each edge has at least one if
   its vertices in S. We will say that an edge e is covered by a node w if one
   of the vertices of e is w.

   Maintain a list of thus-far selected nodes, as well as a count for every
   node. This count is the number of additional edges that will be covered if
   that node is added. For example, say a node w touches three edges, and that
   the other endpoints of these edges are x, y, and z. Say x is already part of
   our list of thus-far selected nodes. Then the count for w would be two,
   because adding w would cover the edges for y and z.

   The algorithm is as follows. The selected node set starts out empty, and the
   count array C is initialized for node w with the number of edges touching w.
   On every iteration:

   * Pick the node w that has the greatest value in the count array.
   * Add w to the selected set.
   * Set the count array at the node w to zero, i.e. C[w] = 0.
   * For every node x that touches w in the graph, decrement C[x] by one.
   * Repeat until there are no (strictly) positive values in the count array C.

   This isn't a solution to the minimum vertex cover, of course, but what we
   really want is to pull together the pqueries that form real clusters. The
   ones with high degree are thus natural to pick first, which is exactly what
   we are doing here.


   == Merging the pqueries ==

   Each original pquery (=node) will then get merged into one of the the
   selected pqueries. This will happen as follows. Maintain a set of unmerged
   pqueries, and a set of pairs (w, d(w)), where w is a selected pquery and
   d(w) is the degree of w in the graph.

   * find the (w, d(w)) pair with the greatest d(w) and remove it from the set

   * find all of the unmerged pqueries that are adjacent to w in the graph, and
   merge them into w. By this I mean take the contatenation of all of their
   namels and add it to the namel for w. If we have mass, then just total all
   of the mass. Remove w and all of those pqueries from the unmerged pquery
   set.

   * repeat!

   * Stop when the unmerged pquery set is empty.

   If the pair-set is exhausted but the unmerged pquery set is not, that's
   reason for an error.

*)

let of_placerun ?(p = 1.) ~c weighting criterion pr =
  let mass_of_pq pq =
    (* Recall that of_pquery_list normalizes out the mass, so that we get a
     * single unit of mass for each pquery. *)
    Mass_map.Pre.of_pquery_list weighting criterion [pq]
      |> Mass_map.Indiv.of_pre
  and gt = Placerun.get_ref_tree pr |> Like_stree.add_zero_root_bl in
  Placerun.get_pqueries pr
  |> Mass_islands.of_pql
  |> List.map (fun (_, pql) ->
    let uptri = List.map mass_of_pq pql
      |> Kr_distance.multi_dist gt p
    and pqa = Array.of_list pql
    and nodem = ref IntMap.empty in
    Uptri.iterij
      (fun i j v ->
        if i = j || v > c then () else (* ... *)
        IntMap.add_listly i j !nodem
          |> IntMap.add_listly j i
          |> (:=) nodem)
      uptri;
    let rec singletons =
      Array.filteri (fun i _ -> not (IntMap.mem i !nodem)) pqa
        |> Array.to_list
    and aux accum nodem =
      if IntMap.is_empty nodem then accum else (* ... *)
      let w, xs = IntMap.enum nodem |> Enum.arg_max (snd |- IntSet.cardinal) in
      let all_touched = IntSet.add w xs in
      let accum' = IntSet.elements xs
        |> List.map (Array.get pqa)
        |> Pquery.merge_into pqa.(w)
        |> flip List.cons accum
      and nodem' = IntSet.fold IntMap.remove all_touched nodem
        |> IntMap.map (flip IntSet.diff all_touched)
      in
      aux accum' nodem'
    in
    IntMap.map IntSet.of_list !nodem |> aux singletons)
  |> List.flatten
