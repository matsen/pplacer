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
open Linear_utils

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
  aux prev_subtot 0. start_data data_info_list

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

(* We'll be using general_total_over_tree to do the heavy lifting. So "data"
   (type 'a) is actually a "thing" that will contain the f_k vector "just
   distal" of the proximal node of the edge, as well as an m_k vector
   describing how much mass for each sample is below that same point. "r" (type
   'b) will be the partial accumulator matrix A for that subtree.

   helper functions:

   curried_edge_total (int -> 'b -> 'a -> 'b * 'a) takes an edge id, the
   previous partial accumulator matrix, the previous (f_k * m_k) tuple/thing,
   and returns a tuple of the new partial accumulator matrix and tuple/thing.
   in the KR distance functions, curried_edge_total is general_total_along_edge
   or total_along_edge curried with the appropriate data; we can maybe do that,
   too, or we can write something special.

   check_final_data ('a -> unit) is just a sanity check for the final
   tuple/thing data, and raises an exception if something is jacked.

   r_list_sum ('b list -> 'b) takes a list of the partial accumulator matrices
   and combines them, and so needs to be an element-by-element sum.

   data_list_sum ('a list -> 'a) takes a list of the tuple/things and combines
   them; how do, and why?

   starter_r_factory (unit -> 'b) initializes the partial accumulator matrix at
   a leaf, so this just needs to return an S x S zero matrix.

   starter_data_factory (unit -> 'a) initializes the tuple/things, so this
   needs to return a tuple/thing with the f_k vector with values initialized to
   M_k (where M_k is the total mass on the tree for sample k) and the m_k
   vector initialized to the zero vector (since all mass on the tree will be
   proximal to x+ at the leaf).

   arguments:

   ref_tree is just the reference tree.

   m is the "thing" describing the mass placements on the tree; I don't know
   what this looks like, exactly.

   return value:

   ('b) Either the raw accumulator matrix A, or (1/S) * A = F'LF. Probably the
   latter.

   *****

   data_to_r ('a -> float -> 'b) takes the "data so far" and a segment length,
   then does something weird: |> merge_r subtotal. What the hell is this?
   EDIT: from the Batteries Included docs, "The (|>) operator becomes like a
   typed unix pipe, feeding the result on the left to the function on the
   right. It reduces the need for unnecessary parentheses and puts the
   computation in the order of actual evaluation." So, data_to_r takes the
   "data so far" and the segment length, computes this segment's partial
   accumulator, and adds it to the previous subtotal (?).

   merge_r ('b -> 'b -> 'b) appears to just be how two objects of type 'b (in
   this case, accumulator matrices) are combined together -- so it's just a
   sum; we can curry total_along_edge instead of specifying this ourselves for
   general_total_along_edge.

   bl (float) is the branch length.

   data_info_list (float * 'c) list is a list of (float * 'c) tuples, where the
   float is the distal branch length, and 'c is some data (in our case, mass
   placements).

   update_data ('a -> 'c -> unit) takes a tuple/thing and a mass placement and
   updates the tuple/thing in-place.

   prev_subtot ('b) is the previous partial accumulator matrix from... where?
   the last segment, or the previous edge? or c) other?

   start_data ('a) is the tuple/thing at the beginning of the edge
   traversal. this will need to be updated from the tuple/thing returned from
   the last child edge, i.e., f_k -= 2*m_k, where m_k is the total mass for
   sample_k below this edge for every branch *except* the one f_k came
   from. confused? good!

   return value:

   ('b * 'a) a tuple of the partial accumulator matrix after traversing the
   edge and the "data so far".
*)

(*
  let recur f_node f_leaf tree =
  let rec aux = function
  | Node(id, tL) -> f_node id (List.map aux tL)
  | Leaf id -> f_leaf id
  in
  aux tree

  recur returns whatever the f_node/f_leaf functions do
*)

(* lpca_make_map takes in a list of maps of placements for the samples, which
   look like

   edge_id -> Mass_map.Indiv.v list

   where Mass_map.Indiv.v is a record of type

   {distal_bl: float; mass: float}

   lpca_make_map merges these maps into a single map of

   edge_id -> (distal_bl * sample_id * mass) list

   where the list is sorted is ascending order of distal_bl.

   TODO: would it be better to make a record for the values rather than use a
   tuple?
*)

let lpca_agg_result l =
  let rec aux x xs =
    match xs with
      | [] -> x
      | y::ys ->
        Gsl_matrix.add x y;
        aux y ys
  in match l with
    | [] -> invalid_arg "lpca_agg_result: empty list"
    | x::xs -> aux x xs

(* intermediate edge result record *)
type lpca_data = { fk: Gsl_vector.vector; mk: Gsl_vector.vector; }

let lpca_agg_data l =
  let rec aux x xs =
    match xs with
      | [] -> failwith "lpca_agg_data: shouldn't happen"
      | y::[] ->
        Gsl_blas.axpy (-2.) x.mk y.fk;
        Gsl_vector.add x.mk y.mk;
        { fk = y.fk; mk = y.mk }
      | y::ys ->
        Gsl_vector.add x.mk y.mk;
        aux y ys
  in match l with
    | [] -> invalid_arg "lpca_agg_data: empty list"
    | x::xs -> aux x xs

let vec_mean v =
  (vec_fold_left (+.) 0. v) /. (float (Gsl_vector.length v))

let vec_denorm v =
  let v_bar = vec_mean v
  in
  vec_map (fun vi -> vi -. v_bar) v

let vec_addi v i x =
  Gsl_vector.set v i ((Gsl_vector.get v i) +. x)

let vec_subi v i x =
  Gsl_vector.set v i ((Gsl_vector.get v i) -. x)

type lpca_placement = {distal_bl: float; sample_id: int; mass: float}

let lpca_tot_edge sm edge_id result data =
  let pl = IntMap.find edge_id sm in
  let rec aux pl prev_distal_bl result data =
    match pl with
      | p::ps ->
        let len = p.distal_bl -. prev_distal_bl
        in
        Gsl_blas.syr Gsl_blas.Upper ~alpha:len ~x:(vec_denorm data.fk) ~a:result;
        vec_subi data.fk p.sample_id (2. *. p.mass);
        vec_addi data.mk p.sample_id p.mass;
        aux ps p.distal_bl result data
      | [] ->
        (result, data)
  in
  aux pl 0. result data

(* f: int -> int -> 'acc_t -> 'p_t list *)
let fold_samples_listwise f acc sl =
  List.fold_left
    (fun (sample_id, acc) s ->
      succ sample_id,
      IntMap.fold
        (* f and fold's arguments are in different orders, otherwise we could
           just curry this *)
        (fun edge_id pl acc -> f sample_id edge_id acc pl)
        s
        acc)
    (0, acc)
    sl |> snd

(* f: int -> int -> 'acc_t -> 'p_t *)
let fold_samples f acc sl =
  fold_samples_listwise
    (fun sample_id edge_id acc pl -> List.fold_left (f sample_id edge_id) acc pl)
    acc
    sl

(* TODO: make sure the incoming lists are sorted properly *)
let make_n_lpca_map sl =
  let repkg_p sample_id {I.distal_bl; I.mass} = {distal_bl; sample_id; mass}
  and cmp_p pa pb = compare pa.distal_bl pb.distal_bl
  in
  fold_samples_listwise
    (fun sample_id edge_id acc pl ->
      IntMap.modify_def
        []
        edge_id
        (List.merge cmp_p (List.map (repkg_p sample_id) pl))
        acc)
    IntMap.empty
    sl

let total_sample_mass sl =
  let mmk = Gsl_vector.create ~init:0. (List.length sl)
  in
  fold_samples
    (fun sample_id _ acc {I.distal_bl; I.mass} ->
      Gsl_vector.set acc sample_id ((Gsl_vector.get acc sample_id) +. mass);
      acc)
    mmk
    sl

let lpca:
    Mass_map.Indiv.v list IntMap.t list -> Stree.t -> 'result_t
  = fun sl ref_tree ->
    let n_samples = List.length sl in
    let result_0 = Gsl_matrix.create ~init:0. n_samples n_samples
    and data_0 = { fk = total_sample_mass sl; mk = Gsl_vector.create ~init:0. n_samples }
    in
    let tot_edge = lpca_tot_edge (make_n_lpca_map sl) in
    let (result, _) =
      Stree.recur
        (fun edge_id node_list -> (* internal nodes *)
          tot_edge
            edge_id
            (lpca_agg_result (List.map fst node_list))
            (lpca_agg_data (List.map snd node_list)))
        (fun edge_id -> (* leaves *)
          tot_edge
            edge_id
            (Gsl_matrix.copy result_0)
            (data_0)) (* FIXME: how do you copy a record? *)
        ref_tree
    in
    result

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
