(* diagd:
 * a class for diagonalized matrices
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

(* here we exponentiate our diagonalized matrix across all the rates.
 * if D is the diagonal matrix, we get a #rates matrices of the form
 * X exp(D rate bl) X^{-1}.
 * util should be a vector of the same length as lambda *)
let multi_exp ~dst x lambda xit util rates bl =
  let n = Gsl_vector.length lambda in
  try
    Tensor.set_all dst 0.;
    for r=0 to (Array.length rates)-1 do
      for i=0 to n-1 do
        set1 util i (exp (rates.(r) *. bl *. (get1 lambda i)))
      done;
      let dst_mat = Tensor.BA3.slice_left_2 dst r in
      Linear.dediagonalize dst_mat x util xit
    done;
  with
    | Invalid_argument s -> invalid_arg ("multi_exp: "^s)

(* here we set up the diagonal entries of the symmetric matrix so
 * that we get the row sum of the Q matrix is zero.
 * see top of code. *)
let b_of_exchangeable_pair m pi =
let n = Gsl_vector.length pi in
  matInit n n
    (fun i j ->
      if i <> j then Gsl_matrix.get m i j
      else
      (* r_ii = - (pi_i)^{-1} \sum_{j \ne i} r_ij pi_j *)
        (let total = ref 0. in
        for k=0 to n-1 do
          if k <> i then
            total := !total +. m.{i,k} *. pi.{k}
        done;
        -. (!total /. pi.{i})))

class diagd arg =
(* See Felsenstein p.206.
 * Say that U \Lambda U^T = D^{1/2} B D^{1/2}.
 * Then DB = (D^{1/2} U) \Lambda (D^{1/2} U)^{-1}
 * Thus we want X = D^{1/2} U, and so
 * X inverse transpose is D^{-1/2} U.
 * *)
  let diagdStructureOfDBMatrix d b =
    let dDiagSqrt = diagOfVec (vecMap sqrt d) in
    let dDiagSqrtInv = diagOfVec (vecMap (fun x -> 1. /. (sqrt x)) d) in
    let (evals, u) = symmEigs (mm dDiagSqrt (mm b dDiagSqrt)) in
    (* make sure that diagonal matrix is all positive *)
    if not (vecNonneg d) then
      failwith("negative element in the diagonal of a DB matrix!");
    (evals, mm dDiagSqrt u, mm dDiagSqrtInv u)
  in

  let (lambdav, x, xit) =
    try
      match arg with
        | `OfData (lambdav, x, xit) -> (lambdav, x, xit)
        | `OfSymmMat m ->
            let evals, evects = symmEigs m in
            (evals, evects, evects)
        | `OfDBMatrix(d, b) -> diagdStructureOfDBMatrix d b
        | `OfExchangeableMat(m, pi) ->
            diagdStructureOfDBMatrix pi (b_of_exchangeable_pair m pi)
    with
      | Invalid_argument s -> invalid_arg ("diagd dimension problem: "^s)
  in
  let util = Gsl_vector.create (Gsl_vector.length lambdav) in

object (self)

  method size = Gsl_vector.length lambdav

  method toMatrix dst = Linear.dediagonalize dst x lambdav xit

  method expWithT dst t =
    Linear.dediagonalize
                  dst
                  x
                  (vecMap (fun lambda -> exp (t *. lambda)) lambdav)
                  xit

  method multi_exp (dst:Tensor.tensor) rates bl =
    multi_exp ~dst x lambdav xit util rates bl

  method normalizeRate pi =
    let q = Gsl_matrix.create (self#size) (self#size) in
    self#toMatrix q;
    let rate = ref 0. in
    for i=0 to (Gsl_vector.length lambdav)-1 do
      rate := !rate -. q.{i,i} *. (Gsl_vector.get pi i)
    done;
    for i=0 to (Gsl_vector.length lambdav)-1 do
      lambdav.{i} <- lambdav.{i} /. !rate
    done;
    ()

end

let ofSymmMat a = new diagd (`OfSymmMat a)
let ofDBMatrix d b = new diagd (`OfDBMatrix(d, b))
let ofExchangeableMat symmPart pi =
  new diagd (`OfExchangeableMat(symmPart, pi))

let normalizedOfExchangeableMat symmPart pi =
  let dd = ofExchangeableMat symmPart pi in
  dd#normalizeRate pi;
  dd

let symmQ n =
  let offDiag = 1. /. (float_of_int (n-1)) in
  matInit n n (fun i j -> if i = j then -. 1. else offDiag)

let symmDQ n = ofSymmMat (symmQ n)
let binarySymmDQ = symmDQ 2

