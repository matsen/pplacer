open Ppatteries

(* Compress pqueries. From the associated github issue:

   A cutoff c is specified via a command line flag. We will be merging pairs of
   pqueries that have KR distance between them less than c.

   The pqueries are put in an array so that they can be referred to by index
   instead of trying to make an OrderedPquery module. In all of the following
   description, these indices and the pqueries are interchangeable.

   To compress the pqueries:

   * divide the pqueries into islands (see mass_islands.ml).

   * for each island, calculate all of the pairwise distances between the
   pqueries for that island and put them in a matrix

   * build a graph such that the nodes are pqueries, and there is an edge
   between them if their distance is less than c (note that this can be a
   binary matrix that is 1 if the distance is less than c and 0 otherwise, an
   easy component-wise application of a function to the pairwise distance
   matrix)

   * merge pqueries according to this graph as described below.


   == Merging the pqueries ==

   Each original pquery (=node) will get merged into one of the the selected
   pqueries. This will happen as follows. Maintain a set of unmerged pqueries,
   and a set of pairs (w, d(w)), where w is a selected pquery and d(w) is the
   degree of w in the graph.

   * find the (w, d(w)) pair with the greatest d(w) and remove it from the set

   * find all of the unmerged pqueries that are adjacent to w in the graph, and
   merge their mass into w. Remove w and all of the adjacent pqueries from the
   unmerged pquery set.

   * repeat!

   * Stop when the unmerged pquery set is empty.

   If the pair-set is exhausted but the unmerged pquery set is not, that's
   reason for an error.

*)

let of_placerun ?(p = 1.) ~c discard_below weighting criterion pr =
  let mass_of_pq pq =
    (* Recall that of_pquery_list normalizes out the mass, so that we get a
     * single unit of mass for each pquery. *)
    Mass_map.Pre.of_pquery_list weighting criterion [pq]
      |> Mass_map.Indiv.of_pre
  and gt = Placerun.get_ref_tree pr |> Like_stree.add_zero_root_bl
  and length = ref 0 in
  Placerun.get_pqueries pr
  |> tap (fun _ -> dprint "Splitting pqueries into islands... ")
  |> Mass_islands.of_pql ~discard_below criterion
  |> tap (fun l -> length := List.length l; dprint "done.\n")
  |> List.mapi (fun i (_, pql) ->
    dprintf "Compressing island %d/%d (%d pqueries)... "
      (succ i)
      !length
      (List.length pql);
    (* For each mass island, make a graph between each pair of pqueries with a
     * KR distance below the `c` threshold. *)
    dprint "kr-dist ";
    let uptri = List.map mass_of_pq pql
      |> Kr_distance.multi_dist gt p
    and pqa = Array.of_list pql
    and nodem = ref IntMap.empty in
    dprint "graph-walk ";
    Uptri.iterij
      (fun i j v ->
        if i = j || v > c then () else (* ... *)
        IntMap.add_listly i j !nodem
          |> IntMap.add_listly j i
          |> (:=) nodem)
      uptri;
    (* ... and collect the nodes in the graph with no edges, since they won't
     * be affected by compression. *)
    let rec singletons =
      Array.filteri (fun i _ -> not (IntMap.mem i !nodem)) pqa
        |> Array.to_list
    and aux accum nodem =
      if IntMap.is_empty nodem then accum else (* ... *)
      (* As long as there are nodes in the graph, find the node with the most
       * edges and all of the adjacent nodes. *)
      let w, xs = IntMap.enum nodem |> Enum.arg_max (snd |- IntSet.cardinal) in
      let all_touched = IntSet.add w xs in
      (* The compressed pquery is the pquery corresponding to the originally
       * selected node with all of the mass from the pqueries corresponding to
       * the adjacent nodes added to it. *)
      let accum' = IntSet.elements xs
        |> List.map (Array.get pqa)
        |> Pquery.merge_into pqa.(w)
        |> flip List.cons accum
      (* And then remove the node and all adjacent nodes from the graph. *)
      and nodem' = IntSet.fold IntMap.remove all_touched nodem
        |> IntMap.map (flip IntSet.diff all_touched)
      in
      aux accum' nodem'
    in
    IntMap.map IntSet.of_list !nodem
      |> aux singletons
      |> tap (fun _ -> dprint "done.\n"))
  |> List.flatten
