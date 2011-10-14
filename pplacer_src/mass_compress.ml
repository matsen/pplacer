open Ppatteries

let of_placerun ?(p = 1.) ~c weighting criterion pr =
  let mass_of_pq pq =
    Mass_map.Pre.of_pquery_list weighting criterion [pq]
      |> Mass_map.Indiv.of_pre
  and gt = Placerun.get_ref_tree pr in
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
    let rec ret = Array.filteri (fun i _ -> not (IntMap.mem i !nodem)) pqa
      |> Array.to_list
    and aux accum used nodem =
      if IntMap.is_empty nodem then accum else (* ... *)
      let w, xs = IntMap.enum nodem |> Enum.arg_max (snd |- IntSet.cardinal) in
      if IntSet.is_empty xs then accum else (* ... *)
      let all_touched = IntSet.add w xs in
      let accum' = IntSet.diff all_touched used
        |> IntSet.elements
        |> List.map (Array.get pqa)
        |> junction
            List.is_empty
            (const accum)
            (Pquery.merge |- flip List.cons accum)
      and used' = IntSet.union all_touched used
      and nodem' = IntMap.add w IntSet.empty nodem
        |> IntSet.fold
            (flip IntMap.modify (IntSet.remove w))
            xs
      in
      aux accum' used' nodem'
    in
    IntMap.map IntSet.of_list !nodem |> aux ret IntSet.empty)
  |> List.flatten
