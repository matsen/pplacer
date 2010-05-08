(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * diagd:
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
 * In this module an "ExchangeableMat" is a speficication of an exchangeability
 * matrix (only the off-diagonal elements count) and the stationary distribution.
 * 
*)

module FGM = Fam_gsl_matvec

let get1 = Bigarray.Array1.unsafe_get
let set1 = Bigarray.Array1.unsafe_set
let get2 = Bigarray.Array2.unsafe_get
let set2 = Bigarray.Array2.unsafe_set

(* deDiagonalize: multiply out eigenvector (u), eigenvalue (lambda) matrices,
 * and inverse eigenvector (uInv) matrices to get usual matrix rep *)
let deDiagonalize ~dst u lambda uinv = 
  let n = Gsl_vector.length lambda in
  try 
    Gsl_matrix.set_all dst 0.;
    for i=0 to n-1 do
      for j=0 to n-1 do
        for k=0 to n-1 do
          (* dst.{i,j} <- dst.{i,j} +. (lambda.{k} *. u.{i,k} *. uInv.{k,j}) *)
          set2 dst i j 
               ((get2 dst i j) +. 
                  (get1 lambda k) *. (get2 u i k) *. (get2 uinv k j))
        done;
      done;
    done;
  with
    | Invalid_argument s -> invalid_arg ("deDiagonalize: "^s)

(* here we exponentiate our diagonalized matrix across all the rates.
 * if D is the diagonal matrix, we get a #rates matrices of the form
 * U exp(D rate bl) U^{-1}. 
 * util should be a vector of the same length as lambda *)
let multi_exp ~dst u lambda uinv util rates bl = 
  let n = Gsl_vector.length lambda in
  try 
    Tensor.set_all dst 0.;
    for r=0 to (Array.length rates)-1 do
      for i=0 to n-1 do
        set1 util i (exp (rates.(r) *. bl *. (get1 lambda i)))
      done;
      let dst_mat = Tensor.BA3.slice_left_2 dst r in
      for i=0 to n-1 do
        for j=0 to n-1 do
          for k=0 to n-1 do
            set2 dst_mat i j
              ((get2 dst_mat i j) +. 
                 (get1 util k) 
                   *. (get2 u i k) 
                   *. (get2 uinv k j))
          done;
        done;
      done;
    done;
  with
    | Invalid_argument s -> invalid_arg ("multi_exp: "^s)


class diagd arg = 
(* See Felsenstein p.206. 
 * Say \Lambda is the diagonal matrix of eigenvalues of
 * D^{1/2} B D^{1/2}. Then Q is
 * (D^{1/2} U) \Lambda (D^{1/2} U)^{-1}.
 * *)
  let diagdStructureOfDBMatrix d b = 
    let dDiagSqrt = FGM.diagOfVec (FGM.vecMap sqrt d) in
    let dDiagSqrtInv = 
      FGM.diagOfVec (FGM.vecMap (fun x -> 1. /. (sqrt x)) d) in
    let (evals, evects) = 
      FGM.symmEigs 
        (FGM.allocMatMatMul dDiagSqrt 
                            (FGM.allocMatMatMul b dDiagSqrt)) in
    (* make sure that diagonal matrix is all positive *)
    if not (FGM.vecNonneg d) then 
      failwith("negative element in the diagonal of a DB matrix!");
    (evals, FGM.allocMatMatMul dDiagSqrtInv evects,
     FGM.allocMatMatMul (FGM.allocMatTranspose evects) dDiagSqrt)
  in

  let (eVals, eVects, invEVects) = 
    try
      match arg with
        | `OfData (evals, evects, invEVects) -> (evals, evects, invEVects)
        | `OfSymmMat m -> 
            let evals, evects = FGM.symmEigs m in
            (evals, evects, FGM.allocMatTranspose evects)
        | `OfDBMatrix(d, b) -> diagdStructureOfDBMatrix d b
        | `OfExchangeableMat(symmPart, statnDist) ->
            let n = Gsl_vector.length statnDist in
            let r = 
              (* here we set up the diagonal entries of the symmetric matrix so
               * that we get the row sum of the Q matrix is zero.
               * see top of code. *)
              FGM.matInit n n (
                fun i j ->
                  if i <> j then Gsl_matrix.get symmPart i j
                  else (
                  (* r_ii = - (pi_i)^{-1} \sum_{j \ne i} r_ij pi_j *)
                    let total = ref 0. in
                    for k=0 to n-1 do
                      if k <> i then 
                        total := !total +. symmPart.{i,k} *. statnDist.{k}
                    done;
                    -. (!total /. statnDist.{i})
                  )
              )
            in 
            diagdStructureOfDBMatrix statnDist r
    with
      | Invalid_argument s -> invalid_arg ("diagd dimension problem: "^s)
  in
  let util = Gsl_vector.create (Gsl_vector.length eVals) in

object (self)

  method size = 
    Gsl_vector.length eVals

  method toMatrix dst = deDiagonalize ~dst eVects eVals invEVects

  method expWithT dst t = 
    deDiagonalize ~dst
                  eVects 
                  (FGM.vecMap (fun lambda -> exp (t *. lambda)) eVals)
                  invEVects

  method multi_exp (dst:Tensor.tensor) rates bl = 
    multi_exp ~dst eVects eVals invEVects util rates bl

  method normalizeRate statnDist = 
    let q = Gsl_matrix.create (self#size) (self#size) in
    self#toMatrix q;
    let rate = ref 0. in
    for i=0 to (Gsl_vector.length eVals)-1 do
      rate := !rate -. q.{i,i} *. (Gsl_vector.get statnDist i)
    done;
    for i=0 to (Gsl_vector.length eVals)-1 do
      eVals.{i} <- eVals.{i} /. !rate
    done;
    ()

end

let ofSymmMat a = new diagd (`OfSymmMat a)
let ofDBMatrix d b = new diagd (`OfDBMatrix(d, b))
let ofExchangeableMat symmPart statnDist = 
  new diagd (`OfExchangeableMat(symmPart, statnDist))

let normalizedOfExchangeableMat symmPart statnDist = 
  let dd = ofExchangeableMat symmPart statnDist in
  dd#normalizeRate statnDist;
  dd

let symmQ n = 
  let offDiag = 1. /. (float_of_int (n-1)) in
  FGM.matInit n n (fun i j -> if i = j then -. 1. else offDiag) 

let symmDQ n = ofSymmMat (symmQ n)
let binarySymmDQ = symmDQ 2

