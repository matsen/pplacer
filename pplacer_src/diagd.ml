(* diagd:
 * a type for diagonalized matrices
 *
 * Time-reversible models are typically speficied by giving the stationary
 * frequency and the "exchangeability" between states.
 * Let D be diag(\pi) and B_{ij} be the exchangeability; note B_{ij} = B_{ji}.
 * We need to set up the diagonal elements of B such that DB is a transition
 * rate matrix, i.e. such that the row sums are zero.
 * Because (DB)_{ij} is \pi_i B_{ij}, we want
 * \pi_i B_{ii} + \sum_{j \ne i} pi_j B_{ij} = 0, i.e.
 * B_{ii} = - (1/pi_i) \sum_{j \ne i} pi_j B_{ij}.
 *
 * From there the DB matrix is easily diagonalized; see Felsenstein p. 206 or
 * pplacer/scans/markov_process_db_diag.pdf.
 *
 * For this module, x and lambdav are such that x diag(lambdav) x^{-1} is the
 * matrix being diagonalized.
 * xit is the inverse transpose of x, which is handy for speedy computations.
 *)

open Fam_gsl_matvec

let mm = allocMatMatMul
let get1 a i = Bigarray.Array1.unsafe_get (a:Gsl_vector.vector) i
let set1 a i = Bigarray.Array1.unsafe_set (a:Gsl_vector.vector) i


(* the setup is that X diag(\lambda) X^{-1} is the matrix of interest. *)
type t =
  {
    x: Gsl_matrix.matrix;
    l: Gsl_vector.vector; (* lambda *)
    xit: Gsl_matrix.matrix; (* x inverse transpose *)
    util: Gsl_vector.vector;
  }

let make ~x ~l ~xit =
  let n = Gsl_vector.length l in
  assert((n,n) = Gsl_matrix.dims x);
  assert((n,n) = Gsl_matrix.dims xit);
  {
    x = x;
    l = l;
    xit = xit;
    util = Gsl_vector.create n
  }


  (* *** utils *** *)

let dim dd = Gsl_vector.length dd.l
let matrix_of_same_dims dd = Gsl_matrix.create (dim dd) (dim dd)


  (* *** into matrices *** *)

let to_matrix dd =
  let m = matrix_of_same_dims dd in
  Linear.dediagonalize m dd.x dd.l dd.xit;
  m

(* return an exponentiated matrix *)
let to_exp ~dst dd bl =
  Gsl_matrix.set_all dst 0.;
  for i=0 to (Gsl_vector.length dd.l)-1 do
    set1 dd.util i (exp (bl *. (get1 dd.l i)))
  done;
  Linear.dediagonalize dst dd.x dd.util dd.xit

(* here we exponentiate our diagonalized matrix across all the rates.
 * if D is the diagonal matrix, we get a #rates matrices of the form
 * X exp(D rate bl) X^{-1}.
 * util should be a vector of the same length as lambda *)
let multi_exp ~dst dd rates bl =
  let n = Gsl_vector.length dd.l in
  try
    Tensor.set_all dst 0.;
    for r=0 to (Array.length rates)-1 do
      for i=0 to n-1 do
        set1 dd.util i (exp (rates.(r) *. bl *. (get1 dd.l i)))
      done;
      let dst_mat = Tensor.BA3.slice_left_2 dst r in
      Linear.dediagonalize dst_mat dd.x dd.util dd.xit
    done;
  with
    | Invalid_argument s -> invalid_arg ("multi_exp: "^s)


  (* *** making *** *)

exception StationaryFreqHasNegativeEntry

let check_stationary v =
  if not (vecNonneg v) then raise StationaryFreqHasNegativeEntry

let of_symmetric m =
  let (l, x) = symmEigs m in
  make ~l ~x ~xit:x

(* d = vector for diagonal, b = symmetric matrix which has been set up with
 * diagonal entries so that BD is a Q-transpose matrix (with zero column
 * totals). see top. *)
(* See Felsenstein p.206.
 * Say that U \Lambda U^T = D^{1/2} B D^{1/2}.
 * Then DB = (D^{1/2} U) \Lambda (D^{1/2} U)^{-1}
 * Thus we want X = D^{1/2} U, and so
 * X inverse transpose is D^{-1/2} U.
 * *)
let of_d_b d b =
  (* make sure that diagonal matrix is all positive *)
  if not (vecNonneg d) then
    failwith("negative element in the diagonal of a DB matrix!");
  let dm_root = diagOfVec (vecMap sqrt d) in
  let dm_root_inv = diagOfVec (vecMap (fun x -> 1. /. (sqrt x)) d) in
  let (l, u) = symmEigs (mm dm_root (mm b dm_root)) in
  make ~l ~x:(mm dm_root_inv u) ~xit:(mm dm_root u)

(* here we set up the diagonal entries of the symmetric matrix so
 * that we get the column sum of the Q matrix is zero.
 * see top of code. *)
let b_of_exchangeable_pair r pi =
let n = Gsl_vector.length pi in
  matInit n n
    (fun i j ->
      if i <> j then Gsl_matrix.get r i j
      else
      (* r_ii = - (pi_i)^{-1} \sum_{k \ne i} r_ki pi_k *)
        (let total = ref 0. in
        for k=0 to n-1 do
          if k <> i then
            total := !total +. r.{k,i} *. pi.{k}
        done;
        -. (!total /. pi.{i})))

let of_exchangeable_pair m pi = of_d_b pi (b_of_exchangeable_pair m pi)

let find_rate dd pi =
  let q = to_matrix dd in
  let rate = ref 0. in
  for i=0 to (dim dd)-1 do
    rate := !rate -. q.{i,i} *. (Gsl_vector.get pi i)
  done;
  !rate

let normalize_rate dd pi =
  Gsl_vector.scale dd.l (1. /. (find_rate dd pi))

let normed_of_exchangeable_pair m pi =
  let dd = of_exchangeable_pair m pi in
  normalize_rate dd pi;
  dd

let symm_q n =
  let off_diag = 1. /. (float_of_int (n-1)) in
  matInit n n (fun i j -> if i = j then -. 1. else off_diag)

let symm_diagd n = of_symmetric (symm_q n)
let binary_symm_diagd = symm_diagd 2
