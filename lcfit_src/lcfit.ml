(* OCaml-C interface to lcfit_tripod *)

module BA = Bigarray
module BA1 = Bigarray.Array1

type float_vector = (float, BA.float64_elt, BA.c_layout) BA1.t

(* The binary symmetric model for a tripod, with one branch length fixed *)
type tripod_bsm = {n00: float; n01: float; n10: float; n11: float;
                   r:   float; b:   float; t:   float; rx:  float;
                   bx:  float}

(* Evaluate the log-likelihood of the BSM given c, tx, and the model *)
external log_like: tripod_bsm -> float -> float -> float = "caml_lcfit_tripod_ll"

(* Fit the BSM given vectors of (c, tx, log_like) and an initial estimate of the
   model *)
external fit: tripod_bsm -> float_vector -> float_vector -> float_vector ->
  tripod_bsm = "caml_lcfit_tripod_fit"

(* Rescale m to intersect with (c, tx, ll) *)
let rescale (c, tx, ll) m =
  let est_ll = log_like m c tx in
  let fac = ll /. est_ll in
  {m with n00=m.n00 *. fac;
    n01=m.n01 *. fac;
    n10=m.n10 *. fac;
    n11=m.n11 *. fac}

let run_test() =
  let test_model = {n00=1500.;n01=300.;n10=300.;n11=300.;r=1.;b=0.5;t=0.390296;rx=1.;bx=0.5} in
  let scaled = rescale (0.1, 0.1, -4000.0) test_model in
  Printf.printf "%f -> %f\n" test_model.n00 scaled.n00
