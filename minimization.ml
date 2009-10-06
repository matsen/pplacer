(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

(* strategies for minimization: 
 * could take one step at a time, with some stepsize
 *
 * *)

exception ExceededMaxIter
exception FoundMin of float

let maxIter = 100

let f x = (x -. 5.) ** 2.

let isOKStart f start left right = 
  let starty = f start in
  starty < f left || starty < f right

let brentOptimization f initStart left right tolerance = 
  (* find a starting point via bisection *)
  let lefty = f left
  and righty = f right in
  let smaller = if lefty < righty then left else right in
  let miny = min lefty righty in
  (* find the start *)
  let rec findStart prevStart iterNum = 
    let prevVal = f prevStart in
    if prevVal < miny then prevStart
    else if abs_float (prevStart -. smaller) < tolerance then
      if prevVal < miny then raise (FoundMin prevStart)
      else raise (FoundMin smaller)
    else if iterNum > maxIter then 
      failwith "guessBrentIter: couldn't find start!"
    else findStart ((prevStart +. smaller) /. 2.) (iterNum+1)
  in
  (* actually do the iteration *)
  try 
    let iterator = 
      Gsl_min.make Gsl_min.BRENT f (findStart initStart 1) left right in
    let rec run whichStep = 
      if whichStep > maxIter then raise ExceededMaxIter
      else (
        Gsl_min.iterate iterator;
        let interLow, interHigh = Gsl_min.interval iterator in
        (* Printf.printf "%f\t%f\t%f\n" (Gsl_min.minimum iterator) interLow
         * interHigh;*)
        if interHigh -. interLow < tolerance then 
          Gsl_min.minimum iterator
        else
          run (whichStep+1)
      )
    in
    run 1
  with
    | FoundMin minLoc -> minLoc


let twoDBrent f startx leftx rightx starty lefty righty tolerance = 
  let rec run currx curry whichStep = 
    let newx = brentOptimization (fun x -> f x curry) currx leftx rightx tolerance in
    let newy = brentOptimization (fun y -> f newx y) curry lefty righty tolerance in
    if whichStep < maxIter && 
       abs_float (currx -. newx) > tolerance &&
       abs_float (curry -. newy) > tolerance 
    then run newx newy (whichStep+1)
    else (f newx newy, newx, newy)
  in
  let result = run startx starty 1 in
  result


let parab x y = 
  let xa = x -. 4. in
  let yb = y -. 4.5 in
  10. *. xa *. xa +. 20. *. yb *. yb *. yb *. yb


(* let z = twoDBrent parab 1. 0. 10. 1. 0. 10. 0.001*)
