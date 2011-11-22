open Ppatteries

let copy_maps_of_gt_map gt leaf_copy_map =
  let open Stree in
  let bl = Gtree.get_bl gt
  and st = Gtree.get_stree gt in
  let rec aux = function
    | Leaf i ->
      Gtree.get_node_label gt i
      |> flip StringMap.find leaf_copy_map
      |> IntMap.singleton i
    | Node (i, subtrees) ->
      let accum = List.fold_left
        (aux |- IntMap.union |> flip)
        IntMap.empty
        subtrees
      in
      IntMap.add
        i
        ((/.)
           (List.map
              (top_id |- (flip IntMap.find accum &&& bl) |- uncurry (/.))
              subtrees
            |> List.fsum)
           (List.map
              (top_id |- bl |- (/.) 1.)
              subtrees
            |> List.fsum))
        accum
  in
  let distal_copy_map = aux st in
  let rec aux accum = function
    | [] -> accum
    | (_, Leaf _) :: rest -> aux accum rest
    | (parent, Node (i, subtrees)) :: rest ->
      let il = List.map top_id subtrees in
      let num, denom = match parent with
        | Some _ -> IntMap.find i accum /. bl i, 1. /. bl i
        | None -> 0., 0.
      in
      let accum' = List.fold_left
        (fun accum j ->
          let rest = List.remove il j in
          IntMap.add
            j
            ((/.)
               (List.map
                  ((flip IntMap.find distal_copy_map &&& bl) |- uncurry (/.))
                  rest
                |> List.cons num
                |> List.fsum)
               (List.map (bl |- (/.) 1.) rest
                |> List.cons denom
                |> List.fsum))
            accum)
        accum
        il
      and rest' = List.fold_left
        (fun accum subtree -> (Some i, subtree) :: accum)
        rest
        subtrees
      in
      aux accum' rest'
  in
  let proximal_copy_map = aux IntMap.empty [None, st] in
  distal_copy_map, proximal_copy_map

let of_criterion_map criterion leaf_copy_map pr =
  let gt = Placerun.get_ref_tree pr in
  let bl = Gtree.get_bl gt
  and dist_cm, prox_cm = copy_maps_of_gt_map gt leaf_copy_map in
  let copy_of_placement p =
    let loc = Placement.location p
    and length = Placement.distal_bl p in
    let bl = bl loc in
    (/.)
      (IntMap.find loc dist_cm /. length
       +. IntMap.find loc prox_cm /. (bl -. length))
      (1. /. length +. 1. /. (bl -. length))
  in
  List.map
    (fun pq ->
      List.map
        ((criterion &&& copy_of_placement) |- uncurry ( *.))
        (Pquery.place_list pq)
      |> List.fsum
      |> (/.) (Pquery.multiplicity pq)
      |> Pquery.set_mass pq)
    (Placerun.get_pqueries pr)
  |> Placerun.set_pqueries pr
