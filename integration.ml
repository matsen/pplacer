(* pplacer v0.2. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)


(*
options are GAUSS15 GAUSS21 GAUSS31 GAUSS41 GAUSS51 GAUSS61, 
corresponding to the 15, 21, 31, 41, 51 and 61 point Gauss-Kronrod rules. 
The higher-order rules give better accuracy for smooth functions, while 
lower-order rules save time when the function contains local difficulties, 
such as discontinuities. 

workspace should be sufficient to hold n double precision intervals, their
integration results and error estimates... depends on number of splits in the
integration.

One commonly distinguishes between the relative error and the absolute error. The absolute error is the magnitude of the difference between the exact value and the approximation. The relative error is the absolute error divided by the magnitude of the exact value. The percent error is the relative error expressed in terms of per 100.
*)


(*
let workspaceSize = 100
let workspace = Gsl_integration.make_ws 100

let valueOfGslResult result = 
  let {Gsl_fun.res = r; Gsl_fun.err = e} = result in r

Gsl_integration.qag f ~a:a ~b:b ~epsabs:absErr ~epsrel:relErr
  ~limit:workspaceSize Gsl_integration.GAUSS61 workspace
 *)

let valueOfTriple (v, err, neval) = v

let pairOfGslResult result = 
  let {Gsl_fun.res = r; Gsl_fun.err = e} = result in (r, e)

(* non-adaptive Gauss-Kronrod integration *)
let integrate f a b absErr relErr = 
  Gsl_integration.qng f ~a:a ~b:b ~epsabs:absErr ~epsrel:relErr 
    
let valueIntegrate f a b absErr relErr = 
  valueOfTriple(integrate f a b absErr relErr)

let twoDIntegrate f leftx rightx lefty righty absErr relErr = 
  valueIntegrate (
    fun y -> valueIntegrate (fun x -> f x y) leftx rightx absErr relErr
  ) lefty righty absErr relErr

(*
 integrateLL: 
 the idea here is to properly integrate log likelihood functions by 
 removing some portion so that when we actually do the integration, we
 don't have underflow problems. this calculates

 baseLL + \log ( \int_a^b \exp(llF - baseLL) dx )
 *)

let integrateLL llF baseLL a b relErr = 
  baseLL +. log(valueIntegrate (fun x -> exp ((llF x) -. baseLL)) a b max_float relErr)

let twoDIntegrateLL llF baseLL leftx rightx lefty righty relErr = 
  baseLL +. log(
    twoDIntegrate (fun x y -> exp ((llF x y) -. baseLL))
      leftx rightx lefty righty max_float relErr)


            (*
let f x = (x -. 5.) ** 2.
let expF x = exp (f x)

let z = log (valueIntegrate expF 0. 1. 0.1 0.1)


let zp = integrateLL f (f 0.) 0. 1. 0.1

(*
 In[5]:= N[Log[Integrate[Exp[-x^2 -y^3], {x,0,1}, {y,2,4}]]]
 Out[5]= -10.8494
 *)

let twoDF x y = -.x*.x -.y*.y*.y
let x = twoDIntegrateLL twoDF (twoDF 0. 2.) 0. 1. 2. 4. 0.1
             *)
