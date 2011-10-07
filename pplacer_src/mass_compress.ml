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
    and pqa = Array.of_list pql in
    let l = Array.length pqa |> pred
    and nodem = ref IntMap.empty in
    (* XXX: figure out what should happen to isolated nodes which don't get
     * added to any part of the map *)
    for i = 0 to l do
      for j = 0 to l do
        match i, j with
          | i, j when i = j -> ()
          | i, j when Uptri.get_loose uptri i j > c -> ()
          | i, j -> nodem := IntMap.add_listly i j !nodem
      done;
    done;
    let rec aux accum nodem =
      let w, xs = IntMap.enum nodem |> Enum.arg_max (snd |- IntSet.cardinal) in
      if IntSet.is_empty xs then accum else (* ... *)
      let all_touching = IntSet.add w xs in
      let accum' =
        IntSet.elements xs
        |> List.map (Array.get pqa)
        |> Pquery.merge_into pqa.(w)
        |> flip List.cons accum
      and nodem' = IntMap.add w IntSet.empty nodem
        |> IntSet.fold
            (* XXX: do we really want to remove all of these, or just w? *)
            (flip IntMap.modify (flip IntSet.diff all_touching))
            xs
      in
      aux accum' nodem'
    in
    IntMap.map IntSet.of_list !nodem |> aux [])
