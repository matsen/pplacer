(* The primary source for length PCA. The code is designed to be read in
parallel with the length_pca tex document.

A couple of "gotchas":
- F is not a matrix of f's. Indeed F is a scaled and centered version of the
matrix of f's:
  F_{i,k} := \frac{1}{s^{1/2}} (f_k(x_i) - \bar f(x_i)),

- The vectors are indexed a bit differently than in the tex document. In the
code, f is a vector indexed by the samples. The location x is typically
implicit, so f.{j} is $f_j(x)$. However, when x is not implicit it is indexed
by i.
*)

open Ppatteries
open Linear_utils

(* Compute the mean of a vector v. *)
let vec_mean v =
  (vec_fold_left (+.) 0. v) /. (float (Gsl.Vector.length v))

(* Compute the mean of a vector v and subtract it from each element, i.e. apply
 * a centering matrix: http://en.wikipedia.org/wiki/Centering_matrix
 *)
let vec_center v =
  let v_bar = vec_mean v in
  vec_map (fun vi -> vi -. v_bar) v

(* Replicate the upper triangle of a matrix m to the lower triangle. *)
let mat_rep_uptri m =
  let (n_rows, _) = Gsl.Matrix.dims m in
  for i=1 to n_rows-1 do
    for j=0 to i-1 do
      m.{i, j} <- m.{j, i}
    done;
  done

type lpca_result = { eval: float array;
                     evect: float array array;
                     edge_evect: float array array }

(* An intermediate edge result record with the following meanings in terms of
   the length_pca writeup. *)
type lpca_data = { f: Gsl.Vector.vector;
                (* $f_k$, the proximal minus the distal mass
                 * (WRT current position). *)
                   ufl: Gsl.Matrix.matrix;
                (* $uFL$, an s x s accumulator matrix of partial inner products
                 * later used in projecting a sample $u$ onto $Fw$. See
                 * eq:ufl. *)
                   af: Gsl.Vector.vector IntMap.t;
                (* $AF$, which is an e x s matrix, here encoded as a map
                 * (edges) to a vector indexed by sample number. See
                 * eq:f_tilde. *)
                   fplf: Gsl.Matrix.matrix }
                (* $F'LF$, which is the "proxy" that we use to avoid computing
                 * the eigenvectors of the full matrix GL. See prop:gl_fplf
                 * and eq:fplf. *)

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
   used to initialize the accumulator to which the other results are
   added. Raise an invalid argument error if we're passed an empty list, as
   that shouldn't happen. *)
let lpca_agg_data l =
  (* Rearranging the definition of f_k(x) and noting that all the mass on the
     tree for a given sample sums to one, we see that the amount of mass m
     distal to a (massless) point x is (1 - f_k(x)) / 2. This relation does not
     hold if there is a placement precisely at x, which is why we assert in
     lpca_tot_edge_nz that no placements occurred precisely at the proximal
     node of the edge. Here b is used for the contribution of a branch. *)
  let attached_mass b =
    let m = Gsl.Vector.copy b.f in
    Gsl.Vector.scale m (-1.);
    Gsl.Vector.add_constant m 1.;
    Gsl.Vector.scale m (1. /. 2.);
    m
  in
  match l with
    | b::bs ->
      let a = List.fold_left
        (fun a bj ->
          (* axpy is y := a*x + y, so the below means a.f += -2 m, where m is
             the amount of mass attached to subtree bj. This accomplishes the
             update described by eq:node_crossing, *)
          Gsl.Blas.axpy (-2.) (attached_mass bj) a.f;
          Gsl.Matrix.add a.ufl bj.ufl;
          (* Add the sibling branch's contribution to F'LF, as in
             eq:piecewise_edge. *)
          Gsl.Matrix.add a.fplf bj.fplf;
          { a with af = map_union a.af bj.af })
        b bs
      in
      a
    | [] ->
      invalid_arg "lpca_agg_data: empty list"

(* Process the placements along an edge of non-zero length from distal to
   proximal, given the initial values data_0 "just beyond" the distal node of the
   edge. *)
let lpca_tot_edge_nz sm edge_id bl data_0 =
  let pl =
    try
      IntMap.find edge_id sm
    with
      | Not_found -> []
  in
  let n_samples = Gsl.Vector.length data_0.f in
  (* af_e is an accumulator for this edge's row in the $\tilde{F} = AF$ matrix,
     defined in eq:f_tilde and later used in computing edge-averaged
     eigenvectors as described in the text. *)
  let af_e = Gsl.Vector.create ~init:0. n_samples in
  (* In aux, i counts the number of "constant regions" along the edge. *)
  let rec aux i pl prev_distal_bl data =
    let update_data sample_id mass len =
      (* f_cen is a convenient alias for the "centered" vector
         { f_1(x) - \bar{f}(x), ..., f_s(x) - \bar{f}(x) }
         the elements of which are used frequently in the tex. *)
      let f_cen = vec_center data.f in
      (* syr is symmetric rank-1 update A = \alpha x x^T + A of the symmetric
       * matrix A. The computation of the update term \sigma_i from eq:sigma_i
       * is "hidden" in this call as the outer product f_cen * f_cen^T, which is
       * then weighted by the region length and added to the F'LF accumulator,
       * as in eq:fplf_update. *)
      Gsl.Blas.syr Gsl.Blas.Upper ~alpha:len ~x:f_cen ~a:data.fplf;
      (* Add this region's contribution to \tilde{F}, as in eq:f_tilde. *)
      Gsl.Vector.add af_e f_cen;
      (* dger is rank-1 update A = \alpha x y^T + A of the matrix A. This is
         the update computation for the inner term of eq:ufl, but for all the
         samples at once. *)
      Gsl.Blas.dger ~alpha:len ~x:data.f ~y:f_cen ~a:data.ufl;
      (* Terminate this region and update f_k as we pass. *)
      data.f.{sample_id} <- data.f.{sample_id} -. (2. *. mass)
    in
    match pl with
      | p::ps ->
        let len = p.distal_bl -. prev_distal_bl in
        (* Make sure we're processing the placements on the edge in the right
           order (i.e. distal to proximal), and that we're not processing
           zero-mass placements. *)
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
        (* Multiplying on left by averaging matrix as just before eq:f_tilde. *)
        Gsl.Vector.scale af_e (1. /. (float (succ i)));
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
  let data_0 = { f = Gsl.Vector.create ~init:1. n_samples; (* prox - distal *)
                 ufl = Gsl.Matrix.create ~init:0. n_samples n_samples;
                 af = IntMap.empty;
                 fplf = Gsl.Matrix.create ~init:0. n_samples n_samples } in
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
          { data_0 with f = Gsl.Vector.copy data_0.f;
                        ufl = Gsl.Matrix.copy data_0.ufl;
                        fplf = Gsl.Matrix.copy data_0.fplf })
      (Gtree.get_stree ref_tree)
  in
  let inv_smo = 1. /. (float (n_samples - 1)) in (* Samples Minus One. *)
  let inv_sqrt_smo = sqrt inv_smo in
  Gsl.Matrix.scale data.fplf inv_smo;
  Gsl.Matrix.scale data.ufl inv_sqrt_smo;
  mat_rep_uptri data.fplf;
  IntMap.iter
    (fun _ v -> Gsl.Vector.scale v inv_sqrt_smo)
    data.af;
  data
