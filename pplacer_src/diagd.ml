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
 * From there the DB matrix is easily diagonalized:
 * Say that U \Lambda U^T = D^{1/2} B D^{1/2}.
 * Then DB = (D^{1/2} U) \Lambda (D^{1/2} U)^{-1}
 * Thus we want X = D^{1/2} U, and so
 * X inverse transpose is D^{-1/2} U.
 * See Felsenstein p. 206.
 *
 * For this module, x and lambdav are such that x diag(lambdav) x^{-1} is the
 * matrix being diagonalized.
 * xit is the inverse transpose of x, which is handy for speedy computations.
 *)

open Linear_utils

let mm = alloc_mat_mat_mul
let get1 a i = Bigarray.Array1.unsafe_get (a:Gsl.Vector.vector) i
let set1 a i = Bigarray.Array1.unsafe_set (a:Gsl.Vector.vector) i


(* the setup is that X diag(\lambda) X^{-1} is the matrix of interest. *)
type t =
  {
    x: Gsl.Matrix.matrix;
    l: Gsl.Vector.vector; (* lambda *)
    xit: Gsl.Matrix.matrix; (* x inverse transpose *)
    util: Gsl.Vector.vector;
  }

let make ~x ~l ~xit =
  let n = Gsl.Vector.length l in
  assert((n,n) = Gsl.Matrix.dims x);
  assert((n,n) = Gsl.Matrix.dims xit);
  {
    x; l; xit;
    util = Gsl.Vector.create n;
  }


  (* *** utils *** *)

let dim dd = Gsl.Vector.length dd.l
let matrix_of_same_dims dd = Gsl.Matrix.create (dim dd) (dim dd)


  (* *** into matrices *** *)

let to_matrix dd =
  let m = matrix_of_same_dims dd in
  Linear.dediagonalize m dd.x dd.l dd.xit;
  m

(* return an exponentiated matrix *)
let to_exp dd bl =
  let dst = matrix_of_same_dims dd in
  for i=0 to (Gsl.Vector.length dd.l)-1 do
    set1 dd.util i (exp (bl *. (get1 dd.l i)))
  done;
  Linear.dediagonalize dst dd.x dd.util dd.xit;
  dst

(* here we exponentiate our diagonalized matrix across all the rates.
 * if D is the diagonal matrix, we get a #rates matrices of the form
 * X exp(D rate bl) X^{-1}.
 * util should be a vector of the same length as lambda.
 * mask is an optional argument that, if specified, only does the multi_exp for
 * those rates that are marked true in the mask. *)
let multi_exp ?mask ~dst dd rates bl =
  let compute =
    match mask with
    | None -> fun _ -> true (* no mask, do everything *)
    | Some m -> fun r -> m.(r) = true (* compute if specified *)
  in
  let n = Gsl.Vector.length dd.l in
  try
    Tensor.set_all dst 0.;
    for r=0 to (Array.length rates)-1 do
      if compute r then begin
        for i=0 to n-1 do
          set1 dd.util i (exp (rates.(r) *. bl *. (get1 dd.l i)))
        done;
        let dst_mat = Tensor.BA3.slice_left_2 dst r in
        Linear.dediagonalize dst_mat dd.x dd.util dd.xit;
      end
    done;
  with
    | Invalid_argument s -> invalid_arg ("multi_exp: "^s)


(* *** making *** *)

exception StationaryFreqHasNegativeEntry

let vec_nonneg v =
  vec_predicate (fun x -> x >= 0.) v

let check_stationary v =
  if not (vec_nonneg v) then raise StationaryFreqHasNegativeEntry

let of_symmetric m =
  let (l, x) = symm_eigs m in
  make ~l ~x ~xit:x

(* If pi_i is zero then we needn't worry about the ith row because it's
 * going to get multiplied by D=diag(pi) on the left, for which the ith row and
 * column will be zero. This should only be used in that case. *)
let safe_invert x = if x = 0. then 1. else 1. /. x

(* d = vector for diagonal, b = symmetric matrix which has been set up with
 * diagonal entries so that BD is a Q-transpose matrix (with zero column
 * totals). See top. *)
let of_d_b d b =
  (* make sure that diagonal matrix is all positive *)
  if not (vec_nonneg d) then
    failwith("negative element in the diagonal of a DB matrix!");
  let d_root = vec_map sqrt d in
  let dm_root = diag d_root in
  let dm_root_inv = diag (vec_map safe_invert d_root) in
  (* Here we multiply on the left by dm_root, justifying use of safe_invert. *)
  let (l, u) = symm_eigs (mm dm_root (mm b dm_root)) in
  make ~l ~x:(mm dm_root_inv u) ~xit:(mm dm_root u)

(* Here we set up the diagonal entries of the symmetric matrix so
 * that we get the column sum of the Q matrix is zero.
 * See top of code.
 * For this to make sense WRT safe_invert one needs to make sure that the
 * resulting matrix gets multiplied on the left by something that zeroes out
 * the ith row for every i that has a zero entry in pi. *)
let b_of_exchangeable_pair r pi =
  let n = Gsl.Vector.length pi in
  mat_init n n
    (fun i j ->
      if i <> j then Gsl.Matrix.get r i j
      else
      (* r_ii = - (pi_i)^{-1} \sum_{k \ne i} r_ki pi_k *)
        (let total = ref 0. in
        for k=0 to n-1 do
          if k <> i then
            total := !total +. r.{k,i} *. pi.{k}
        done;
        -. (!total *. (safe_invert pi.{i}))))

(* Note that of_d_b multiplies m on the left by the square root of diag(pi),
 * justifying use of safe_invert. *)
let of_exchangeable_pair m pi = of_d_b pi (b_of_exchangeable_pair m pi)

let find_rate dd pi =
  let q = to_matrix dd in
  let rate = ref 0. in
  for i=0 to (dim dd)-1 do
    rate := !rate -. q.{i,i} *. (Gsl.Vector.get pi i)
  done;
  !rate

let normalize_rate dd pi =
  Gsl.Vector.scale dd.l (1. /. (find_rate dd pi))

let normed_of_exchangeable_pair m pi =
  let dd = of_exchangeable_pair m pi in
  normalize_rate dd pi;
  dd

let symm_q n =
  let off_diag = 1. /. (float_of_int (n-1)) in
  mat_init n n (fun i j -> if i = j then -. 1. else off_diag)

let symm_diagd n = of_symmetric (symm_q n)
let binary_symm_diagd = symm_diagd 2
