open Ppatteries
include Gsl_matrix

let n_rows = dims |- fst
let n_cols = dims |- snd

let ppr ff m =
  let last = n_rows m - 1 in
  Format.fprintf ff "@[{";
  for i=0 to last do
    Linear_utils.ppr_gsl_vector ff (row m i);
    if i < last then Format.fprintf ff ";@ "
  done;
  Format.fprintf ff "}@]"
