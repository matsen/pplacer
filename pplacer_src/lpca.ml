open Ppatteries
open Linear_utils

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

   { distal_bl: float; mass: float }

   lpca_make_map merges these maps into a single map of

   edge_id -> (distal_bl * sample_id * mass) list

   where the list is sorted is ascending order of distal_bl.
*)

(* intermediate edge result record *)
type lpca_data = { fk: Gsl_vector.vector; mk: Gsl_vector.vector }

(* repackaged placement record which includes the sample id *)
type lpca_placement = { distal_bl: float; sample_id: int; mass: float }

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

let lpca_tot_edge sm edge_id result data =
  let pl =
    try
      IntMap.find edge_id sm
    with
      | Not_found -> []
  in
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

(* f: int -> int -> 'acc_t -> 'p_t list -> 'acc_t *)
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

(* f: int -> int -> 'acc_t -> 'p_t -> 'acc_t *)
let fold_samples f acc sl =
  fold_samples_listwise
    (fun sample_id edge_id acc pl -> List.fold_left (f sample_id edge_id) acc pl)
    acc
    sl

let make_n_lpca_map sl =
  let repkg_p sample_id { Mass_map.Indiv.distal_bl; Mass_map.Indiv.mass } = { distal_bl; sample_id; mass }
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

(* TODO: the Mass_map module actually has functionality for doing this -- use
   that instead and pass it to gen_lpca *)
let total_sample_mass sl =
  let mmk = Gsl_vector.create ~init:0. (List.length sl)
  in
  fold_samples
    (fun sample_id _ acc { Mass_map.Indiv.distal_bl; Mass_map.Indiv.mass } ->
      Gsl_vector.set acc sample_id ((Gsl_vector.get acc sample_id) +. mass);
      acc)
    mmk
    sl

let gen_lpca sl ref_tree =
  let sm = make_n_lpca_map sl in
  let tot_edge = lpca_tot_edge sm in
  let n_samples = List.length sl in
  let result_0 = Gsl_matrix.create ~init:0. n_samples n_samples
  and data_0 = { fk = total_sample_mass sl; mk = Gsl_vector.create ~init:0. n_samples } in
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
