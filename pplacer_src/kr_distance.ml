(* Here we have the core agorithm, which takes two placement collection lists
 * and calculates their weighted KR distance.
 *
 * Each query is allocated 1/(num queries), and this get broken up by the
 * different placements according to their weight. Note that if a placement
 * collection has only one entry (list of length one) then all of the weight
 * will go onto that placement. This will be true when we are not doing the
 * weighted version.
 *
 * These KR info vectors get totalled as we proceed up the tree, and their
 * exponentiated difference gets multiplied by the length of the segment without
 * any placements. This gives the total KR distance.
*)

open Fam_batteries
open MapsSets

exception Invalid_place_loc of float
exception Total_kr_not_zero of float

let tol = 1e-10 (* "zero" *)

(* the KR exponent is 1/p unless p is less than 1, in which case it's 1 *)
let outer_exponent p =
  if p < 1. then 1.
  else 1. /. p

(* exp_kr_diff is the difference of the two prob dists to the pth pow *)
let exp_kr_diff p kr_v = (abs_float (kr_v.(0) -. kr_v.(1))) ** p

(* add v2 to v1 (which is modified in place) *)
let v_addto v1 v2 =
  for i=0 to (Array.length v1)-1 do
    v1.(i) <- v1.(i) +. v2.(i)
  done

(* take the vector sum of a float array list. no side effects. *)
let v_list_sum = function
  | hd::tl ->
    let v = Array.copy hd in
    List.iter (v_addto v) tl;
    v
  | [] -> assert(false)

(* Total up the info from the previous step.
 * note that data_sofar will be modified in place.
 * data_to_r: takes the kr info vector and turns it into a real number
 * data_info_list: list of (distal_bl, data)
 *
 * Signature is
('a -> float) -> float -> (float * 'b) list -> ('a -> 'b -> 'c) -> float ->     'a          -> float * 'a
 data_to_r       bl       data_info_list        update_data        prev_subtot  start_data
 *
 * is what ocaml infers, but update_data is actually 'a -> 'b -> unit, as
 * update_data modifies in place.
 * *)
let total_along_edge data_to_r bl data_info_list update_data prev_subtot start_data =
  let rec aux ~subtotal ~prev_a data_sofar data_info_list =
    (* next_subtotal actually adds on the segment length times data_to_r of the
     * kr vector *)
    let next_subtotal a =
      let seg_len = a -. prev_a in
      assert(seg_len >= 0.);
      subtotal+.seg_len*.(data_to_r data_sofar)
    in
    match data_info_list with
    (* a is the location of the location of the data along the edge *)
    | (a, data)::rest ->
        (* we pull this out so that we do the next total, then add on the data
         * onto the data_sofar *)
        if a < 0. || a > bl then raise (Invalid_place_loc a);
        let the_next_subtotal = next_subtotal a in
        aux
          ~subtotal:the_next_subtotal
          ~prev_a:a
          (update_data data_sofar data; data_sofar)
          rest
    | [] ->
        (* sum things up on final segment to the next node *)
        (next_subtotal bl, data_sofar)
  in
  aux prev_subtot 0. start_data data_info_list

(* total some data over the tree, which can be combined from subtrees using
 * data_list_sum, and can be totaled across edges using curried_edge_total, and
 * starts at leaves with starter_data_factory.
 * the reason why we use starter_data_factory rather than doing a fully
 * functional approach is that then the number of allocations is linear in only
 * the size of the tree, rather than depending on the number of placements.
 * *)
let total_over_tree curried_edge_total
                    check_final_data
                    data_list_sum
                    starter_data_factory
                    ref_tree =
  let (grand_total, final_data) =
    Stree.recur
      (fun id below_list -> (* the node recurrence *)
        curried_edge_total
          id
          (List.fold_right ( +. ) (List.map fst below_list) 0.) (* prev subtot *)
          (data_list_sum (List.map snd below_list))) (* total of below kr_infos *)
      (fun id -> (* the leaf recurrence *)
        curried_edge_total
          id
          0.
          (starter_data_factory ()))
      (Gtree.get_stree ref_tree)
  in
  check_final_data final_data;
  grand_total

(* combine two float list IntMaps into a single float 2-array list IntMap.
 * The latter is the input for the KR distance function. *)
let make_kr_map m1 m2 =
  let process_map f =
    IntMap.map (List.map (fun (dist_bl, mass) -> (dist_bl, f mass)))
  in
  Mass_map.Indiv.sort
    (Base.combine_list_intmaps
    [
      process_map (fun mass -> [|mass; 0.|]) m1;
      process_map (fun mass -> [|0.; mass|]) m2;
    ])

(* Z_p distance between two Indiv mass maps *)
let dist ?(normalization=1.) ref_tree p m1 m2 =
  let starter_kr_v = [|0.; 0.|]
  and kr_map = make_kr_map m1 m2 in
  let kr_edge_total id =
    total_along_edge
      (exp_kr_diff p)
      (Gtree.get_bl ref_tree id)
      (Base.get_from_list_intmap id kr_map)
      v_addto
  (* make sure that the kr_v totals to zero *)
  and check_final_kr final_kr_v =
    let final_kr_diff = final_kr_v.(0) -. final_kr_v.(1) in
    if abs_float final_kr_diff > tol then
      raise (Total_kr_not_zero final_kr_diff)
  in
  ((total_over_tree
      kr_edge_total
      check_final_kr
      v_list_sum
      (fun () -> Array.copy starter_kr_v)
      ref_tree)
    /. normalization)
  ** (outer_exponent p)

(* x1 and x2 are factors which get multiplied by the mass before calculation.
 * By pulling them out like so, we don't have to make new Pres. *)
let dist_of_pres ?x1 ?x2 ?(normalization=1.) transform p t ~pre1 ~pre2 =
  dist ~normalization
    t
    p
    (Mass_map.Indiv.of_pre transform ?factor:x1 pre1)
    (Mass_map.Indiv.of_pre transform ?factor:x2 pre2)

let scaled_dist_of_pres ?(normalization=1.) transform p t pre1 pre2 =
  dist_of_pres ~normalization transform p t
    ~x1:(1. /. Mass_map.Pre.total_mass transform pre1)
    ~x2:(1. /. Mass_map.Pre.total_mass transform pre2)
    ~pre1 ~pre2
