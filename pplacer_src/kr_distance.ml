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

open Ppatteries

exception Invalid_place_loc of float
exception Total_kr_not_zero of float

let tol = 1e-10 (* "zero" *)

(* the KR exponent is 1/p unless p is less than 1, in which case it's 1 *)
let outer_exponent p =
  if p < 1. then 1.
  else 1. /. p

(* exp_kr_diff is the difference of the two prob dists to the pth pow *)
let exp_kr_diff_times_bl p kr_v bl = ((abs_float (kr_v.(0) -. kr_v.(1))) ** p) *. bl

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
  | [] -> invalid_arg "v_list_sum"

(* Total up the info from the previous step.
 * note that data_sofar will be modified in place.
 * data_to_r: takes the kr info vector and turns it into a result
 * merge_r: combine two results
 * data_info_list: list of (distal_bl, data)
 *)
let general_total_along_edge:
    ('a -> float -> 'b) -> merge_r:('b -> 'b -> 'b) -> float -> (float * 'c) list ->
    ('a -> 'c -> unit) -> 'b -> 'a -> 'b * 'a
= fun data_to_r ~merge_r bl data_info_list update_data prev_subtot start_data ->
  let rec aux ~subtotal ~prev_a data_sofar data_info_list =
    (* next_subtotal actually adds on the segment length times data_to_r of the
     * kr vector *)
    let next_subtotal a =
      let seg_len = a -. prev_a in
      assert(seg_len >= 0.);
      data_to_r data_sofar seg_len |> merge_r subtotal
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
  aux ~subtotal:prev_subtot ~prev_a:0. start_data data_info_list

(* total some data over the tree, which can be combined from subtrees using
 * data_list_sum, and can be totaled across edges using curried_edge_total, and
 * starts at leaves with starter_data_factory. the result starts at
 * starter_r_factory and is passed to curried_edge_total and totalled across
 * nodes using r_list_sum.
 * the reason why we use starter_data_factory rather than doing a fully
 * functional approach is that then the number of allocations is linear in only
 * the size of the tree, rather than depending on the number of placements.
 * *)
let general_total_over_tree:
    (int -> 'b -> 'a -> 'b * 'a) -> ('a -> unit) ->
    r_list_sum:('b list -> 'b) -> ('a list -> 'a) ->
    starter_r_factory:(unit -> 'b) -> (unit -> 'a) ->
    Stree.t -> 'b
= fun curried_edge_total
      check_final_data
      ~r_list_sum
      data_list_sum
      ~starter_r_factory
      starter_data_factory
      ref_tree
->
  let (grand_total, final_data) =
    Stree.recur
      (fun id below_list -> (* the node recurrence *)
        curried_edge_total
          id
          (r_list_sum (List.map fst below_list)) (* prev subtot *)
          (data_list_sum (List.map snd below_list))) (* total of below kr_infos *)
      (fun id -> (* the leaf recurrence *)
        curried_edge_total
          id
          (starter_r_factory ())
          (starter_data_factory ()))
      ref_tree
  in
  check_final_data final_data;
  grand_total

(* woooo eta expansion *)
let total_along_edge a b c d e f =
  general_total_along_edge
    ~merge_r:(+.)
    a b c d e f
let total_over_tree a b c d e =
  general_total_over_tree
    ~r_list_sum:(List.fold_left (+.) 0.)
    ~starter_r_factory:(const 0.)
    a b c d e

module I = Mass_map.Indiv

(* N Indiv.v list IntMaps -> (float * float N-array) list IntMap.
 * The latter is the input for the KR distance function. *)
let make_n_kr_map ml =
  let ll = List.length ml in
  let arr e v = let a = Array.make ll 0. in a.(e) <- v; a in
  List.fold_left
    (fun (e, accum) cur ->
      succ e,
      IntMap.merge
        (fun _ ao bo ->
          Option.map_default
            (List.map
               (fun {I.distal_bl; I.mass} -> distal_bl, arr e mass))
            []
            bo
          |> List.merge (comparing fst) (Option.default [] ao)
          |> some)
        accum
        (I.sort cur))
    (0, IntMap.empty)
    ml
  |> snd

(* Z_p distance between two Indiv mass maps *)
let dist ?(normalization=1.) ref_tree p m1 m2 =
  let starter_kr_v = [|0.; 0.|]
  and kr_map = make_n_kr_map [m1; m2] in
  let kr_edge_total id =
    total_along_edge
      (exp_kr_diff_times_bl p)
      (Gtree.get_bl ref_tree id)
      (IntMap.get id [] kr_map)
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
      (Gtree.get_stree ref_tree))
    /. normalization)
  ** (outer_exponent p)

(* x1 and x2 are factors which get multiplied by the mass before calculation.
 * By pulling them out like so, we don't have to make new Pres. *)
let dist_of_pres ?x1 ?x2 ?(normalization=1.) p t ~pre1 ~pre2 =
  dist ~normalization
    t
    p
    (Mass_map.Indiv.of_pre ?factor:x1 pre1)
    (Mass_map.Indiv.of_pre ?factor:x2 pre2)

let scaled_dist_of_pres ?(normalization=1.) p t pre1 pre2 =
  dist_of_pres ~normalization p t
    ~x1:(1. /. Mass_map.Pre.total_mass pre1)
    ~x2:(1. /. Mass_map.Pre.total_mass pre2)
    ~pre1 ~pre2


let uptri_exp_kr_diff_times_bl p kr_v bl =
  Uptri.init
    (Array.length kr_v)
    (fun i j -> ((abs_float (kr_v.(i) -. kr_v.(j))) ** p) *. bl)

(* Z_p distance between any number of Indiv mass maps *)
let multi_dist ?(normalization=1.) ref_tree p ml =
  let starter_kr_v = Array.make (List.length ml) 0. in
  let starter_kr_uptri = Uptri.create (Array.length starter_kr_v) 0.
  and kr_map = make_n_kr_map ml in
  let kr_edge_total id =
    general_total_along_edge
      ~merge_r:(Uptri.apply_pairwise (+.))
      (uptri_exp_kr_diff_times_bl p)
      (Gtree.get_bl ref_tree id)
      (IntMap.get id [] kr_map)
      v_addto
  (* make sure that every pair of kr_v totals are equal *)
  and check_final_kr final_kr_v =
    Array.to_list final_kr_v
      |> ListFuns.list_iter_over_pairs_of_single
          (fun x y ->
            if not (approx_equal x y) then
              raise (Total_kr_not_zero (x -. y)))
  in
  general_total_over_tree
    kr_edge_total
    check_final_kr
    ~r_list_sum:(List.fold_left (Uptri.apply_pairwise (+.)) starter_kr_uptri)
    v_list_sum
    ~starter_r_factory:(fun () -> Uptri.copy starter_kr_uptri)
    (fun () -> Array.copy starter_kr_v)
    (Gtree.get_stree ref_tree)
  |> Uptri.map (fun v -> (v /. normalization) ** (outer_exponent p))

let scaled_multi_dist_of_pres ?(normalization=1.) p t prel =
  multi_dist ~normalization t p
    (List.map
       (fun pre ->
         Mass_map.Indiv.of_pre
           ~factor:(1. /. Mass_map.Pre.total_mass pre)
           pre)
       prel)
