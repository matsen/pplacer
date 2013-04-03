exception ExceededMaxIter
exception FoundMin of float

let f x = (x -. 5.) ** 2.

let isOKStart f start left right =
  let starty = f start in
  starty < f left || starty < f right

(* run the GSL Brent algorithm. the work below is to find an appropriate
 * starting point, i.e. a location x such that f(x) < min(f(left), f(right)).
 *)
let brent ?(max_iters=100) f raw_start left right tolerance =
  if left >= raw_start || raw_start >= right then
    failwith
    (Printf.sprintf
      "Minimization.brent: start values don't satisfy %g < %g < %g"
      left raw_start right);
  (* find a starting point via bisection. *)
  let lefty = f left
  and righty = f right in
  let smaller = if lefty < righty then left else right in
  let miny = min lefty righty in
  (* find the start *)
  let rec find_start prevStart iterNum =
    let prevVal = f prevStart in
    if prevVal < miny then
      (* we have found an appropriate starting point *)
      prevStart
    else if abs_float (prevStart -. smaller) < tolerance then
      (* we are already within the specified tolerance via bisection *)
      if prevVal < miny then raise (FoundMin prevStart)
      else raise (FoundMin smaller)
    else if iterNum > max_iters then
      failwith "Minimization.brent: couldn't find start!"
    else
      (* bisect *)
      find_start ((prevStart +. smaller) /. 2.) (iterNum+1)
  in
  (* actually do the iteration *)
  try
    let iterator =
        Gsl_min.make Gsl_min.BRENT f (find_start raw_start 1) left right in
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
