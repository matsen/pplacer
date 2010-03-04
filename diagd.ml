(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * diagd:
 * a class for diagonalized matrices
 *
*)

module FGM = Fam_gsl_matvec

let get1 = Bigarray.Array1.get
let set1 = Bigarray.Array1.set
let get2 = Bigarray.Array2.get
let set2 = Bigarray.Array2.set

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

let multi_exp ~dst u lambda uinv rates bl = 
  let n = Gsl_vector.length lambda in
  let exp_lambda = Gsl_vector.create n in
  try 
    Gsl_matrix.set_all dst 0.;
    for r=0 to (Array.length rates)-1 do
      for i=0 to n-1 do
        set1 exp_lambda i (exp (rates.(r) *. bl *. (get1 lambda i)))
      done;
      let rate_bump = n * r in
      for i=0 to n-1 do
        for j=0 to n-1 do
          let dst_column = rate_bump+j in
          for k=0 to n-1 do
            set2 dst i dst_column
              ((get2 dst i dst_column) +. 
                 (get1 exp_lambda k) 
                   *. (get2 u i k) 
                   *. (get2 uinv k j))
          done;
        done;
      done;
    done;
  with
    | Invalid_argument s -> invalid_arg ("multi_exp: "^s)



class diagd arg = 
  (* if we have a matrix of the form B times D, where D is diag and B is symmetric *)
  let diagdStructureOfBDMatrix b d = 
    let dDiagSqrt = FGM.diagOfVec (FGM.vecMap sqrt d) in
    let dDiagSqrtInv = FGM.diagOfVec (
      FGM.vecMap (fun x -> 1. /. (sqrt x)) d) in
    let (evals, evects) = FGM.symmEigs (
      FGM.allocMatMatMul dDiagSqrt (FGM.allocMatMatMul b dDiagSqrt)) in
    (* make sure that diagonal matrix is all positive *)
    if not (FGM.vecNonneg d) then 
      failwith("negative element in the diagonal of a BD matrix!");
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
        | `OfBDMatrix(b, d) -> diagdStructureOfBDMatrix b d
        | `OfExchangeableMat(symmPart, statnDist) ->
            let n = Gsl_vector.length statnDist in
            let r = 
              (* here we set up the diagonal entries of the symmetric matrix so
               * that we get the row sum of the Q matrix is zero *)
              FGM.matInit n n (
                fun i j ->
                  if i <> j then Gsl_matrix.get symmPart i j
                  else (
                    (* r_ii = - (pi_i)^{-1} \sum_{j \neq i} r_ij pi_j *)
                    let total = ref 0. in
                    for k=0 to n-1 do
                      if k <> i then 
                        total := !total +. symmPart.{i,k} *. statnDist.{k}
                    done;
                    -. (!total /. statnDist.{i})
                  )
              )
            in 
            diagdStructureOfBDMatrix r statnDist
    with
      | Invalid_argument s -> invalid_arg ("diagd dimension problem: "^s)
  in

object (self)

  method size = 
    Gsl_vector.length eVals

  method toMatrix dst = deDiagonalize ~dst eVects eVals invEVects

  method expWithT dst t = 
    deDiagonalize ~dst
                  eVects 
                  (FGM.vecMap (fun lambda -> exp (t *. lambda)) eVals)
                  invEVects

  method multi_exp dst rates bl = 
    multi_exp ~dst eVects eVals invEVects rates bl

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
let ofBDMatrix b d = new diagd (`OfBDMatrix(b, d))
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

