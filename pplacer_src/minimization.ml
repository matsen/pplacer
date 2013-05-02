open Ppatteries

exception ExceededMaxIter
exception FindStartFailure
(* (left, start, right)*)
exception InvalidStartValues of float * float * float
exception FoundMin of float
exception FoundStart of float


(* A little bit of prep work for our start_finders - DRY *)
let start_finder_prep f left raw_start right =
  if left >= raw_start || raw_start >= right then
    raise (InvalidStartValues (left, raw_start, right));
  let lefty, righty = f left, f right in
  let smaller = if lefty < righty then left else right
  and miny = min lefty righty in
  (smaller, miny)


(* This function attempts to find a starting point through bisection in the
 * direction of the boundary (right, left) which has the lower value. This is a
 * safe assumption for likehood functions, but not for the multimin work.
 *)
let bisection_start_finder ?(max_iters=100) f raw_start left right tolerance =
  let smaller, miny = start_finder_prep f left raw_start right in
  let rec finder prevStart iterNum =
    let prevVal = f prevStart in
    if prevVal < miny then
      (* we have found an appropriate starting point *)
      prevStart
    else if abs_float (prevStart -. smaller) < tolerance then
      (* we are already within the specified tolerance via bisection *)
      if prevVal < miny then raise (FoundMin prevStart)
      else raise (FoundMin smaller)
    else if iterNum > max_iters then
      raise FindStartFailure
    else
      (* bisect *)
      finder ((prevStart +. smaller) /. 2.) (iterNum+1)
  in
  finder raw_start 1


(* This start finder tries to avoid the assumptions inherent in the
 * bisection_start_finder by searching uniformly throughout the the bounding
 * interval, jumping back and forth on either side of the raw_start.
 *)
let robust_start_finder ?(max_iters=100) f raw_start left right _ =
  try
    let _, miny = start_finder_prep f left raw_start right in
    if f raw_start < miny then raise (FoundStart raw_start)
    else
      (* This chunk of code jumps back and forth on either side of the raw_start
       * by a constant increment to try and find a suitable start that is as
       * close as possible to the raw start. This is helpful for preserving the
       * sign of coefficients in SOM. *)
      let finder samples =
        let incr_ratio i = (right -. left) *. (float i) /. (float samples +. 1.)
        (* jump back and forth between negative and positive integers *)
        and jump_indices = Enum.init (2 * samples) (fun i -> (Int.pow (-1) i) * ((i+2)/2))
        and check_start x =
          if (f x) < miny then raise (FoundStart x)
        in
        (* map those integers to their input values, and filter to make sure in
         * left right*)
        let jump_enum = Enum.filter
          (fun x -> left < x & x < right)
          (Enum.map (fun i -> raw_start +. (incr_ratio i)) jump_indices)
        in
        Enum.iter check_start jump_enum
      in
      finder max_iters;
    raise FindStartFailure
  with
  | FoundStart start -> start


(* run the GSL Brent algorithm - the Ggsl_min algorithms require that the
 * starting point satisfy f(x) < min(f(left), f(right)).
 * value evaluate to a lower value than the evaluation at the bounding points,
 * and this function has an argument for how you want to do that.
 *)
let brent ?(max_iters=100) ?(start_finder=bisection_start_finder) f raw_start left right tolerance =
  try
    let start = start_finder ~max_iters f raw_start left right tolerance in
    let iterator =
        Gsl_min.make Gsl_min.BRENT f start left right in
    let rec run whichStep =
      if whichStep > max_iters then raise ExceededMaxIter
      else begin
        Gsl_min.iterate iterator;
        let interLow, interHigh = Gsl_min.interval iterator in
        if interHigh -. interLow < tolerance then
          Gsl_min.minimum iterator
        else
          run (whichStep+1)
      end
    in
    run 1
  with
  | FoundMin minLoc -> minLoc


(* No max iteration checking going on here yet... *)
let multimin ?(max_iters=100) obj_fun start lower_bounds upper_bounds tolerance =
  let dims = Array.init (Array.length start) (fun i -> i) in
  let sub_iterator start' dim =
    let start'' = Array.copy start' in
    let input x =
      Array.set start'' dim x;
      start''
    in
    let obj_part x = obj_fun (input x) in
    let min = brent
      ~start_finder:robust_start_finder
      obj_part start'.(dim)
      lower_bounds.(dim)
      upper_bounds.(dim)
      tolerance
    in
    input min
  in
  let iterator start' = (start', Array.fold_left sub_iterator start' dims) in
  let rec run (input1, input2) step =
    if (step > max_iters) then raise ExceededMaxIter
    else if ((obj_fun input1) -. (obj_fun input2) > tolerance) then
      run (iterator input2) (step+1)
    else
      input2
  in
  run (iterator start) 1


