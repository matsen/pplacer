open Ppatteries

module SAMR = AlgMapR (StringMap)
let sorted_tuple ?(cmp = compare) (a, b) = if cmp a b < 0 then b, a else a, b

let of_pql criterion pql =
  let map = ref Map.empty in
  List.fold_left
    (fun accum pq ->
      let name = Pquery.name pq in
      List.fold_left
        (fun accum p ->
          IntMap.modify_def
            SAMR.empty
            (Placement.location p)
            (SAMR.add_by name (criterion p))
            accum)
        accum
        (Pquery.place_list pq))
    IntMap.empty
    pql
  |> IntMap.iter (fun _ samr ->
    let names, weights = SAMR.enum samr
    |> Enum.uncombine
    |> Tuple2.mapn Array.of_enum Array.of_enum
    in
    Uptri.init
      (Array.length names)
      (fun i j -> weights.(i) *. weights.(j))
    |> Uptri.iterij
        (fun i j w ->
          let k = sorted_tuple (names.(i), names.(j)) in
          map := Map.modify_def 0. k ((+.) w) !map));
  Map.enum !map
  |> Enum.map (fun ((n1, n2), v) -> n1, n2, v)
