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
