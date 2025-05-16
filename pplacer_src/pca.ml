open Ppatteries

let tol = 1e-14
let max_iter = 100000


(* *** general PCA stuff *** *)

let dot = ArrayFuns.fold_left2 (fun s x1 x2 -> s +. (x1 *. x2)) 0.

(* Returns array of values, and then array of GSL vectors (i.e. left
 * eigenmatrix if considered as a matrix). Just keep the top n_keep
 * eigenpairs.
 * Eigenvalues returned as a float array, and eigenvects as an array of GSL
 * vectors. *)
let power_eigen n_keep m =
  let open Power_iteration in
  let eiga = top_eigs m tol max_iter n_keep in
  (Array.map (fun e -> e.l) eiga, Array.map (fun e -> e.v) eiga)

(* Alternative version that uses symmv rather than power iteration. *)
let symmv_eigen n_keep m =
  let (evalv, evectm) = Gsl.Eigen.symmv (`M (m)) in
  Gsl.Eigen.symmv_sort (evalv, evectm) Gsl.Eigen.ABS_DESC;
  (* GSL makes nice column vectors *)
  Gsl.Matrix.transpose_in_place evectm;
  (Array.sub (Gsl.Vector.to_array evalv) 0 n_keep,
  Array.init n_keep (Gsl.Matrix.row evectm))

(* Pass in an n by p array of arrays, and make the corresponding p by p
 * sample covariance matrix (i.e. such that divisor is n-1). Scale determines
 * if entries should be divided by the product of the standard deviations to
 * get a matrix of sample correlation coefficients.
 * Eigenvalues returned as a float array, and eigenvects as an array of GSL
 * vectors. *)
let covariance_matrix ?scale faa =
  let x = Gsl.Matrix.of_arrays faa
  and inv k = 1./.(float_of_int k)
  in
  let (n, p) = Gsl.Matrix.dims x in
  let h = Linear_utils.mat_init n n
            (fun i j -> if i=j then 1.-.(inv n) else -.(inv n))
  and a = Gsl.Matrix.create n p
  and cov = Gsl.Matrix.create p p
  in
  Linear_utils.mat_mat_mul a h x;
  Gsl.Blas.syrk Gsl.Blas.Upper Gsl.Blas.Trans ~alpha:(inv (n-1)) ~a:a ~beta:0. ~c:cov;
  let scale_f = match scale with
    | None | Some false -> (fun x _ _ -> x)
    | Some true ->
      let scalefact = Linear_utils.vec_init p (fun i -> 1./.sqrt(cov.{i,i})) in
      (fun x i j -> x *. scalefact.{i} *. scalefact.{j})
  in
  for i=0 to p-1 do
    for j=i to p-1 do
      cov.{i,j} <- scale_f cov.{i,j} i j;
      cov.{j,i} <- cov.{i,j};
    done;
  done;
  cov

(* Return (evals, evects), where only the top n_keep are kept.
 * Optionally, scale the eigenvalues by the trace of the covariance matrix.
 * Don't forget that the covariance matrix is positive definite, thus the
 * eigenvalues are positive, so eigenvalue divided by the trace is the
 * "fraction" of the variance "explained" by that principal component.
 * Eigenvects returned as a float array array.
 * *)
let gen_pca ?(symmv=false) ?(use_raw_eval=false) ?scale n_keep faa =
  let eigen = if symmv then symmv_eigen else power_eigen in
  let cov = covariance_matrix ?scale faa in
  let (raw_evals, raw_evects) = eigen n_keep cov in
  let evects = Array.map Gsl.Vector.to_array raw_evects in
  if use_raw_eval then (raw_evals, evects)
  else
    (let tr = Linear_utils.trace cov in
    Array.map (fun eval -> assert(eval < tr); eval /. tr) raw_evals,
    evects)
