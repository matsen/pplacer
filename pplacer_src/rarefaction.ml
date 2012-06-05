open Ppatteries

let merge = const incr |> flip
let lmerge = List.fold_left ((!) |- (+) |> flip) 0 |- ref

module I = Mass_map.Indiv

(* Convenience wrapper around the functions in Kr_distance for specifying just
 * a callback with parameters for the count of edge segments below the current
 * edge segment and the branch length. The return values of the callbacks are
 * summed across the tree. *)
let count_along_mass gt mass cb =
  let partial_total id =
    Kr_distance.total_along_edge
      cb
      (Gtree.get_bl gt id)
      (IntMap.get id [] mass |> List.map I.to_pair |> List.sort compare)
      merge
  in
  Kr_distance.total_over_tree
    partial_total
    (const ())
    lmerge
    (fun () -> ref 0)
    (Gtree.get_stree gt)

let k_maps_of_placerun: int -> Newick_bark.t Placerun.t -> float IntMap.t IntMap.t
= fun k_max pr ->
  let n = Placerun.get_pqueries pr |> List.length in
  let n' = float_of_int n
  and base_k_map = 0 -- n
    |> Enum.map (identity &&& const 1.)
    |> IntMap.of_enum
    |> IntMap.singleton 0
  in
  Enum.fold
    (fun k_maps k ->
      let prev_map = IntMap.find k k_maps
      and diff = n' -. float_of_int k in
      let q_k r = (diff -. float_of_int r) /. diff *. IntMap.find r prev_map in
      0 -- n
        |> Enum.map (identity &&& q_k)
        |> IntMap.of_enum
        |> flip (IntMap.add (k + 1)) k_maps)
    base_k_map
    (0 --^ k_max)

(* Compute the rarefaction curve of a placerun, given a placement criterion and
 * optionally the highest X value for the curve. *)
let of_placerun:
    (Placement.t -> float) -> ?k_max:int -> Newick_bark.t Placerun.t -> (int * float) Enum.t
= fun criterion ?k_max pr ->
  let gt = Placerun.get_ref_tree pr |> Newick_gtree.add_zero_root_bl
  and mass = I.of_placerun
    Mass_map.Point
    criterion
    pr
  in
  let n = Placerun.get_pqueries pr |> List.length in
  let k_max = match k_max with
    | Some k when k < n -> k
    | _ -> n
  in
  let k_maps = k_maps_of_placerun k_max pr in
  let count k =
    let q_k = IntMap.find k k_maps |> flip IntMap.find in
    count_along_mass
      gt
      mass
      (fun d bl ->
        let d = !d in
        let p = n - d in
        (1. -. (q_k d) -. (q_k p)) *. bl)
  in
  2 -- k_max
    |> Enum.map (identity &&& count)
