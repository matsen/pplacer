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

let vec_print s v =
  Format.fprintf
    Format.std_formatter
    "%s: %a@."
    s
    Linear_utils.ppr_gsl_vector v;
  v

let mat_print s m =
  Format.fprintf
    Format.std_formatter
    "%s: %a@."
    s
    Linear_utils.ppr_gsl_matrix m;
  m

let vec_mean v =
  (vec_fold_left (+.) 0. v) /. (float (Gsl_vector.length v))

let vec_denorm v =
  let v_bar = vec_mean v in
  vec_map (fun vi -> vi -. v_bar) v

let vec_addi v i x =
  Gsl_vector.set v i ((Gsl_vector.get v i) +. x)

let mat_addi m i j x =
  Gsl_matrix.set m i j ((Gsl_matrix.get m i j) +. x)

let mat_rep_uptri m =
  let (n_rows, _) = Gsl_matrix.dims m in
  for i=1 to n_rows-1 do
    for j=0 to i-1 do
      Gsl_matrix.set m i j (Gsl_matrix.get m j i)
    done;
  done

type lpca_result = { eval: float array; evect: float array array; edge_evect: float array array }

(* intermediate edge result record *)
type lpca_data = { fk: Gsl_vector.vector;
                   mk: Gsl_vector.vector;
                   ufl: Gsl_matrix.matrix;
                   af: Gsl_vector.vector IntMap.t;
                   fplf: Gsl_matrix.matrix }

(* repackaged placement record which includes the sample id *)
type lpca_placement = { distal_bl: float; sample_id: int; mass: float }

let map_union m1 m2 =
  IntMap.merge
    (fun _ l r ->
      match l, r with
        | Some x, None
        | None, Some x ->
          Some x
        | Some _, Some _ ->
          invalid_arg "map_union: maps intersect"
        | _ ->
          invalid_arg "map_union: Something Bad happened")
    m1 m2

let lpca_agg_data l =
  match l with
    | x::xs ->
      let a = List.fold_left
        (fun a xi ->
          Gsl_vector.add a.mk xi.mk;
          Gsl_matrix.add a.ufl xi.ufl;
          Gsl_matrix.add a.fplf xi.fplf;
          { a with af = map_union a.af xi.af })
        { x with mk = Gsl_vector.create ~init:0. (Gsl_vector.length x.mk) }
        xs
      in
      Gsl_blas.axpy (-2.) a.mk a.fk;
      Gsl_vector.add a.mk x.mk;
      a
    | [] ->
      invalid_arg "lpca_agg_data: empty list"

let lpca_tot_edge_nz sm edge_id bl data_0 =
  let pl =
    try
      IntMap.find edge_id sm
    with
      | Not_found -> []
  in
  let n_samples = Gsl_vector.length data_0.fk in
  let af_e = Gsl_vector.create ~init:0. n_samples in
  let rec aux i pl prev_distal_bl data =
    let update_data sample_id mass len =
      let fk' = vec_denorm data.fk in
      Gsl_blas.syr Gsl_blas.Upper ~alpha:len ~x:fk' ~a:data.fplf;
      vec_addi data.fk sample_id ((-2.) *. mass);
      vec_addi data.mk sample_id mass;
      Gsl_vector.add af_e fk';
      (* FIXME: this is terribly inefficient *)
      for k=0 to n_samples-1 do
        for j=0 to n_samples-1 do
          mat_addi data.ufl k j ((Gsl_vector.get data.fk k) *. (Gsl_vector.get fk' j) *. len);
        done;
      done
    in
    match pl with
      | p::ps ->
        let len = p.distal_bl -. prev_distal_bl in
        (* Make sure we're processing the placements on the edge in the right
           order, and that we're not processing zero-mass placements. *)
        assert(p.distal_bl >= prev_distal_bl);
        assert(p.distal_bl < bl);
        assert(p.mass > 0.);
        update_data p.sample_id p.mass len;
        aux (succ i) ps p.distal_bl data
      | [] ->
        let len = bl -. prev_distal_bl in
        (* We should never be dealing with a zero-length edge, nor should we
           have processed a placement exactly at the proximal node of the edge, so
           len must always be greater than zero. *)
        assert(len > 0.);
        (* TODO: make this smarter *)
        update_data 0 0. len;
        Gsl_vector.scale af_e (1. /. (float (succ i)));
        { data with af = IntMap.add edge_id af_e data.af }
  in
  aux 0 pl 0. data_0

let lpca_tot_edge sm edge_id bl data_0 =
  if bl > 0. then (lpca_tot_edge_nz sm edge_id bl data_0) else data_0

(* f: int -> int -> 'acc_t -> 'p_t list -> 'acc_t *)
let fold_samples_listwise f a sl =
  List.fold_left
    (fun (sample_id, a) s ->
      succ sample_id,
      IntMap.fold
        (fun edge_id pl a -> f sample_id edge_id a pl) s a)
    (0, a)
    sl |> snd

let make_n_lpca_map sl =
  let repkg_p sample_id { Mass_map.Indiv.distal_bl; Mass_map.Indiv.mass } = { distal_bl; sample_id; mass }
  and cmp_p pa pb = compare pa.distal_bl pb.distal_bl in
  (* TODO: check for placements where distal_bl = bl -- should those be moved to a parent edge? *)
  fold_samples_listwise
    (fun sample_id edge_id a pl ->
      IntMap.modify_def
        []
        edge_id
        (List.merge cmp_p
           (List.map (repkg_p sample_id)
              (List.filter (fun { Mass_map.Indiv.mass } -> mass > 0.) pl)))
        a)
    IntMap.empty
    sl

let total_sample_mass sl =
  let mmk = Gsl_vector.create ~init:0. (List.length sl) in
  List.iteri (fun i si -> Gsl_vector.set mmk i (Mass_map.Indiv.total_mass si)) sl;
  mmk

(* TODO: Does guppy always scale the total mass on the tree to 1.0? If so, we
   don't need to bother computing the total mass for each sample. But what if
   the total masses for each sample are actually different? *)
let gen_data sl ref_tree =
  let sm = make_n_lpca_map sl in
  let tot_edge = lpca_tot_edge sm in
  let n_samples = List.length sl in
  let data_0 = { fk = total_sample_mass sl;
                 mk = Gsl_vector.create ~init:0. n_samples;
                 ufl = Gsl_matrix.create ~init:0. n_samples n_samples;
                 af = IntMap.empty;
                 fplf = Gsl_matrix.create ~init:0. n_samples n_samples } in
  let data =
    Stree.recur
      (fun edge_id dl -> (* internal nodes *)
        tot_edge
          edge_id
          (Gtree.get_bl ref_tree edge_id)
          (lpca_agg_data dl))
      (fun edge_id -> (* leaves *)
        tot_edge
          edge_id
          (Gtree.get_bl ref_tree edge_id)
          { data_0 with fk = Gsl_vector.copy data_0.fk;
                        mk = Gsl_vector.copy data_0.mk;
                        ufl = Gsl_matrix.copy data_0.ufl;
                        fplf = Gsl_matrix.copy data_0.fplf })
      (Gtree.get_stree ref_tree)
  in
  let inv_smo = 1. /. (float (n_samples - 1)) in
  let inv_sqrt_smo = sqrt inv_smo in
  Gsl_matrix.scale data.fplf inv_smo;
  Gsl_matrix.scale data.ufl inv_sqrt_smo;

  mat_rep_uptri data.fplf;
  IntMap.iter
    (fun _ v -> Gsl_vector.scale v inv_sqrt_smo)
    data.af;
  data
