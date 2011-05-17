(* calculate a pairwise distance matrix between placements.
 * a "numbered distal map" is a map going from location to (distal_bl,
 * placement_number) where placement_number is the number to be used in the
 * distance matrix.
*)

open MapsSets

let ppr_dm = Uptri.ppr_uptri (Ppr.ppr_opt Ppr.ppr_gfloat)

 (* sort in order going towards the root *)
let sort_numbered_distal_map_by_distal =
 IntMap.map (List.sort (fun (a1,_) (a2,_) -> compare a1 a2))

(* placemap should be a map from location to a list of placements at that
 * location.
 * NOTE: due to an order of evaluation bug in map.ml, the order of the
 * numberings is not what I would have expected. in fact, it's reverse. it
 * doesn't matter for functionality, but it does for checking in the toplevel.
 * *)
let numbered_distal_map_of_placemap start_num placemap =
  let num = ref start_num in
  let number_place p =
    let result = (Placement.distal_bl p, !num) in
    (* Printf.printf "loc %d has %d\n" (Placement.location p) (!num); *)
    incr num;
    result
  in
  (!num,
    sort_numbered_distal_map_by_distal
      (IntMap.map (List.map number_place) placemap))

(* pa is an array of placements; the numbering comes from the array index *)
let numbered_distal_map_of_place_arr pa =
  sort_numbered_distal_map_by_distal
    (IntMap.of_pairlist_listly
      (Array.to_list
        (Array.mapi
          (fun i p ->
            (Placement.location p,
              (Placement.distal_bl p,i))) pa)))

(* here we actually do the work. i decided to process the placements along an
 * edge in three stages, as described in the definition below starting
 * "order matters." The reason I did that rather than having a recursion going
 * along the edge is that then we aren't adding a little snippet of edge to
 * dist_to_here each time we encounter a new placement. This might be important
 * with many placements. And it's just as easy. *)
let of_numbered_distal_map t n_p ndm =
  let dist_to_here = Array.make n_p 0. in
  let add_to_dth i x = dist_to_here.(i) <- dist_to_here.(i) +. x in
  (* we are going to be carrying tokens up the tree, i.e. the collection of
   * placement numbers which are below the edge numbered id. accu are the active
   * tokens. dist_to_here are the distances to the present edge. here we update
   * dth and return accu @ (the new tokens) *)
  let update_dth_and_accu bl accu numbered_ps =
    (* carry along the active tokens *)
    List.iter (fun i -> add_to_dth i bl) accu;
    (* pass on the old *)
    accu @
    (* and add on the new *)
      (List.map
        (fun (attach_loc, tok_num) ->
          (* after adding their dists to dist_to_here *)
          add_to_dth tok_num (bl -. attach_loc);
          tok_num)
        numbered_ps)
  in
  let dm = Uptri.create n_p None in
  let set_dm i j x = Uptri.set_loose dm i j (Some x) in
  (* record the distances which are below a given internal node *)
  let process_internal_node below =
    Base.list_list_iterpairs
      (fun i j ->
        set_dm i j (dist_to_here.(i) +. dist_to_here.(j)))
      below
  in
  (* record the distances which are along a given edge *)
  let rec process_edge below = function
    | [] -> ()
    | (distal_bl, num)::above ->
  (* record the distances from this one to others above on this edge *)
      List.iter
        (fun (above_distal_bl,above_num) ->
          set_dm num above_num (above_distal_bl -. distal_bl))
        above;
(* record distances from this one to the ones below this edge's internal node *)
      List.iter
        (fun below_num ->
          set_dm num below_num (distal_bl +. dist_to_here.(below_num)))
        below;
(* recur for the rest *)
      process_edge below above;
  in
  let _ =
(* the downside to using the recur function is some clear code dup below *)
    Gtree.recur
(* --- internal nodes --- *)
      (fun id below ->
        let bl = Gtree.get_bl t id in
        (* the numbered placement distal_bls *)
        let numbered_ps = Base.get_from_list_intmap id ndm in
        let flat_below = List.flatten below in
  (* order important:
   * first record the distances between the taxa below this edge *)
        process_internal_node below;
  (* then record the distances which are along a given edge *)
        process_edge flat_below numbered_ps;
  (* then add on this edge's length to the dist_to_here *)
        update_dth_and_accu bl flat_below numbered_ps)
(* --- leaves --- *)
      (fun id ->
        let bl = Gtree.get_bl t id in
        let numbered_ps = Base.get_from_list_intmap id ndm in
        process_edge [] numbered_ps;
        update_dth_and_accu bl [] numbered_ps)
      t
  in
  Uptri.map
    (function
      | Some x -> x
      | None ->
          Format.fprintf Format.std_formatter "%a@." ppr_dm dm;
          failwith "empty entry in distance matrix!")
    dm

let of_placement_list_map t pl_map =
  let (n_p, ndm) = numbered_distal_map_of_placemap 0 pl_map in
  of_numbered_distal_map t n_p ndm

let of_placement_array t pa =
  of_numbered_distal_map
    t
    (Array.length pa)
    (numbered_distal_map_of_place_arr pa)

