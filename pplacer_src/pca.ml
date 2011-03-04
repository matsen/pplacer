open MapsSets
open Fam_batteries


(* *** general PCA stuff *** *)

let dot = ArrayFuns.fold_left2 (fun s x1 x2 -> s +. (x1 *. x2)) 0.

(* returns array of values, and then array of vectors (i.e. left eigenmatrix if
  * considered as a matrix) *)
let my_symmv m =
  let (evalv, evectm) = Gsl_eigen.symmv (`M (m)) in
  Gsl_eigen.symmv_sort (evalv, evectm) Gsl_eigen.VAL_DESC;
  (* GSL makes nice column vectors *)
  Gsl_matrix.transpose_in_place evectm;
  (Gsl_vector.to_array evalv, Gsl_matrix.to_arrays evectm)

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

(* make an array of (eval, evect) tuples. Keep only the top n_keep. *)
let gen_pca ?n_keep ?scale faa =
  let (vals, vects) as system = my_symmv (covariance_matrix ?scale faa) in
  match n_keep with
  | None -> system
  | Some n -> (Array.sub vals 0 n, Array.sub vects 0 n)



