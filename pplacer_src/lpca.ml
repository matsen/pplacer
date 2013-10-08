open Ppatteries
open Linear_utils

(* Compute the mean of a vector v. *)
let vec_mean v =
  (vec_fold_left (+.) 0. v) /. (float (Gsl_vector.length v))

(* Compute the mean of a vector v and subtract it from each element, i.e. apply
 * a centering matrix: http://en.wikipedia.org/wiki/Centering_matrix
 *)
let vec_center v =
  let v_bar = vec_mean v in
  vec_map (fun vi -> vi -. v_bar) v

(* Replicate the upper triangle of a matrix m to the lower triangle. *)
let mat_rep_uptri m =
  let (n_rows, _) = Gsl_matrix.dims m in
  for i=1 to n_rows-1 do
    for j=0 to i-1 do
      m.{i, j} <- m.{j, i}
    done;
  done

type lpca_result = { eval: float array;
                     evect: float array array;
                     edge_evect: float array array }

(* An intermediate edge result record. These have the following meanings in
 * terms of the length_pca writeup. All vectors are indexed by the sample
 * number, so for example fk.{i} is for the ith number. It's a little confusing
 * because the subscript k refers to the sample number, but this way we can
 * have a pretty close relationship between the code and the write-up.
 *)
type lpca_data = { fk: Gsl_vector.vector;
                (* $f_k$, the proximal minus the distal mass
                 * (WRT current position). *)
                   mk: Gsl_vector.vector;
                (* $m_k$, the mass accumulator. *)
                   ufl: Gsl_matrix.matrix;
                (* $uFL$, an s x s accumulator matrix of partial inner products
                 * later used in projecting a sample $u$ onto $Fw$. *)
                   af: Gsl_vector.vector IntMap.t;
                (* $AF$, which is an e x s matrix, here encoded as a map
                 * (edges) to a vector indexed by sample number. *)
                   fplf: Gsl_matrix.matrix }
                (* $F'LF$ *)

(* A repackaged placement record which includes the sample id. *)
type lpca_placement = { distal_bl: float;
                        sample_id: int;
                        mass: float }

(* Compute the union of two (disjoint) maps. If the maps are not disjoint,
   raise an invalid argument error. *)
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

(* Aggregate a list of intermediate results from subtrees. The first result is
   used to initialize the accumulator to which the other results are added,
   with the exception of the per-sample distal mass, which is only accumulated
   *after* the current region's sample mass difference vector data.fk is
   updated. Raise an invalid argument error if we're passed an empty list, as
   that shouldn't happen. *)
let lpca_agg_data l =
  match l with
    | x::xs ->
      let a = List.fold_left
        (fun a xi ->
          Gsl_vector.add a.mk xi.mk; (* a.{m_k} += x.{m_k} *)
          Gsl_matrix.add a.ufl xi.ufl;
          Gsl_matrix.add a.fplf xi.fplf;
          { a with af = map_union a.af xi.af })
        { x with mk = Gsl_vector.create ~init:0. (Gsl_vector.length x.mk) }
        (* Above: zero out the mass accumulator before folding. *)
        xs
      in
      Gsl_blas.axpy (-2.) a.mk a.fk; (* axpy is y := a*x + y.
                           Here a.f_k += -2 a.m_k as we are going past $m_k$. *)
      Gsl_vector.add a.mk x.mk; (* When we process the next edge proximal to
                                   this one we'll need to know how much total
                                   mass is distal to that edge, so we must add
                                   x.mk to the accumulator a.mk at some
                                   point. However, since we used the edge data
                                   record x as the initial value of the
                                   aggregation, we do this *after* we've
                                   aggregated the $f_k$ values into a.fk, since
                                   the mass on x was already considered when
                                   computing x.fk. *)
      a
    | [] ->
      invalid_arg "lpca_agg_data: empty list"

(* Process the placements along an edge of non-zero length, given the initial
   values data_0 "just beyond" the distal node of the edge. *)
let lpca_tot_edge_nz sm edge_id bl data_0 =
  let pl =
    try
      IntMap.find edge_id sm
    with
      | Not_found -> []
  in
  let n_samples = Gsl_vector.length data_0.fk in
  (* af_e is an accumulator for this edge's row in the $\tilde{F} = AF$ matrix,
     which is later used in computing edge-averaged eigenvectors as described in
     the text. *)
  let af_e = Gsl_vector.create ~init:0. n_samples in
  (* In aux, i counts the number of "constant regions" along the edge. *)
  let rec aux i pl prev_distal_bl data =
    let update_data sample_id mass len =
      let fk' = vec_center data.fk in
      (* syr is symmetric rank-1 update A = \alpha x x^T + A of the symmetric
       * matrix A. *)
      Gsl_blas.syr Gsl_blas.Upper ~alpha:len ~x:fk' ~a:data.fplf;
      data.fk.{sample_id} <- data.fk.{sample_id} -. (2. *. mass);
      data.mk.{sample_id} <- data.mk.{sample_id} +. mass;
      Gsl_vector.add af_e fk';
      (* dger is rank-1 update A = \alpha x y^T + A of the matrix A. *)
      Gsl_blas.dger ~alpha:len ~x:data.fk ~y:fk' ~a:data.ufl
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
           have processed a placement exactly at the proximal node of the edge,
           so len must always be greater than zero. *)
        assert(len > 0.);
        update_data 0 0. len;
        Gsl_vector.scale af_e (1. /. (float (succ i)));
        (* Above: multiplying on left by averaging matrix. *)
        { data with af = IntMap.add edge_id af_e data.af }
  in
  aux 0 pl 0. data_0

(* Process the placements along an edge, given the initial values data_0 "just
   beyond" the distal node of the edge. If the edge is of zero length, simply
   return data_0. *)
let lpca_tot_edge sm edge_id bl data_0 =
  if bl > 0. then (lpca_tot_edge_nz sm edge_id bl data_0) else data_0

(* Accumulate a function f across a list of samples into the accumulator a. *)
let fold_samples_listwise f a sl =
  List.fold_left
    (fun (sample_id, a) s ->
      succ sample_id,
      IntMap.fold
        (fun edge_id pl a -> f sample_id edge_id a pl) s a)
    (0, a)
    sl |> snd

(* Repackage the list of samples into a map indexed by edge where the
   placements are sorted in increasing order of distal length along that edge,
   so that lpca can walk the edge from distal to proximal and process the
   placements as they're encountered. Also filter out any placements of zero
   mass, because we don't care about them. *)
let make_n_lpca_map sl =
  let repkg_p sample_id { Mass_map.Indiv.distal_bl; Mass_map.Indiv.mass } = { distal_bl; sample_id; mass }
  and cmp_p pa pb = compare pa.distal_bl pb.distal_bl in
  (* TODO: check for placements where distal_bl = bl -- should those be moved
     to a parent edge? *)
  fold_samples_listwise
    (fun sample_id edge_id a pl ->
      IntMap.modify_def
        [] (* If edge_id not a key of the map, insert []. *)
        edge_id (* Key to modify. *)
        (List.merge cmp_p
           (List.map (repkg_p sample_id)
              (List.filter (fun { Mass_map.Indiv.mass } -> mass > 0.) pl)))
           (* Function to apply to the value of edge_id in the map. *)
        a)
    IntMap.empty
    sl

(* The heart of the lpca algorithm. First we initialize an intermediate result
   record data_0 which can be used to initialize the algorithm at any leaf on
   the tree, then we process each edge from distal to proximal and update the
   intermediate results via lpca_tot_edge and aggregate those results across
   subtrees via lpca_agg_data. Once the tree has been traversed, we scale the
   final result record's components appropriately and return it. *)
let gen_data sl ref_tree =
  let sm = make_n_lpca_map sl in
  let tot_edge = lpca_tot_edge sm in
  let n_samples = List.length sl in
  let data_0 = { fk = Gsl_vector.create ~init:1. n_samples; (* prox - distal *)
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
  let inv_smo = 1. /. (float (n_samples - 1)) in (* Samples Minus One. *)
  let inv_sqrt_smo = sqrt inv_smo in
  Gsl_matrix.scale data.fplf inv_smo;
  Gsl_matrix.scale data.ufl inv_sqrt_smo;
  mat_rep_uptri data.fplf;
  IntMap.iter
    (fun _ v -> Gsl_vector.scale v inv_sqrt_smo)
    data.af;
  data
