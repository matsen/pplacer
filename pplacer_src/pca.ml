open Ppatteries

let tol = 1e-15
let max_iter = 100000


(* *** general PCA stuff *** *)

let dot = ArrayFuns.fold_left2 (fun s x1 x2 -> s +. (x1 *. x2)) 0.

(* Returns array of values, and then array of vectors (i.e. left eigenmatrix if
  * considered as a matrix). Just keep the top n_keep eigenpairs. *)
let power_eigen n_keep m =
  let eiga = Power_iteration.top_eigs m tol max_iter n_keep in
  (Array.map (fun e -> e.Power_iteration.l) eiga,
   Array.map (fun e -> Gsl_vector.to_array (e.Power_iteration.v)) eiga)

(* Alternative version that uses symmv rather than power iteration. *)
let symmv_eigen n_keep m =
  let (evalv, evectm) = Gsl_eigen.symmv (`M (m)) in
  Gsl_eigen.symmv_sort (evalv, evectm) Gsl_eigen.ABS_DESC;
  (* GSL makes nice column vectors *)
  Gsl_matrix.transpose_in_place evectm;
  let sub a = Array.sub a 0 n_keep in
  (sub (Gsl_vector.to_array evalv), sub (Gsl_matrix.to_arrays evectm))

let column aa j = Array.init (Array.length aa) (fun i -> aa.(i).(j))

(* pass in an a by n array of arrays, and make the corresponding n by n
 * covariance matrix. this is the standard way, such that rows represent
 * observations and columns represent variables.
 * Assuming rectangularity, etc, and clearly not highly optimized.
 * *)
let covariance_matrix ?scale faa =
  let n = Array.length faa.(0) in
  let m = Gsl_matrix.create n n
  and col = Array.init n (column faa)
  in
  let base_cov i j = Gsl_stats.covariance col.(i) col.(j) in
  let f = match scale with
    | None | Some false -> base_cov
    | Some true ->
        let dia = Array.init n (fun i -> sqrt(base_cov i i)) in
        (fun i j ->
          let num = base_cov i j
          and denom = dia.(i) *. dia.(j)
          in
          if denom = 0. then (assert (num = 0.); 0.)
          else num /. denom)
  in
  for i=0 to n-1 do
    for j=i to n-1 do
      let cov = f i j in
      m.{i,j} <- cov;
      m.{j,i} <- cov;
    done;
  done;
  m

(* Return (evals, evects), where only the top n_keep are kept.
 * Optionally, scale the eigenvalues by the trace of the covariance matrix.
 * Don't forget that the covariance matrix is positive definite, thus the
 * eigenvalues are positive, so eigenvalue divided by the trace is the
 * "fraction" of the variance "explained" by that principal component.
 * *)
let gen_pca ?(symmv=false) ?(use_raw_eval=false) ?scale ?right_mul_mat n_keep faa =
  let eigen = if symmv then symmv_eigen else power_eigen in
  let pre_cov = covariance_matrix ?scale faa in
  let cov = match right_mul_mat with
    | None -> pre_cov
    | Some x -> Linear_utils.alloc_mat_mat_mul pre_cov x
  in
  let (raw_evals, evects) = eigen n_keep cov in
  if use_raw_eval then (raw_evals, evects)
  else
    (let tr = Linear_utils.trace cov in
    Array.map (fun eval -> assert(eval < tr); eval /. tr) raw_evals,
    evects)

