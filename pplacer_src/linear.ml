(* just the calls for linear_c.
*)

module BA = Bigarray
module BA1 = BA.Array1
module BA2 = BA.Array2

external glv_print : Tensor.tensor -> unit = "glv_print_c"

(* statd x y z util *)
external log_like3 : Gsl_vector.vector -> Tensor.tensor -> Tensor.tensor -> Tensor.tensor -> Gsl_vector.vector -> float = "log_like3_c"

(* dst x y *)
external pairwise_prod : Tensor.tensor -> Tensor.tensor -> Tensor.tensor -> unit = "pairwise_prod_c"

(* dst = a * b *)
external gemmish : Gsl_matrix.matrix -> Gsl_matrix.matrix -> Gsl_matrix.matrix -> unit = "gemmish_c"

(* statd dst a b *)
external statd_pairwise_prod : Gsl_vector.vector -> Tensor.tensor -> Tensor.tensor -> Tensor.tensor -> unit = "statd_pairwise_prod_c"

(* x y first last util
 * take the logarithm of the dot product of x and y restricted to the interval
 * [start, last]. start and last are 0-indexed, of course.
 * *)
external bounded_logdot : Tensor.tensor -> Tensor.tensor -> int -> int -> Gsl_vector.vector -> float = "bounded_logdot_c"

(* dst u lambda uit
 * where uit is u inverse transpose
 * dst_ij = sum_k (lambda_k *. u_ik *. uit_jk)
 * *)
external dediagonalize : Gsl_matrix.matrix -> Gsl_matrix.matrix -> Gsl_vector.vector -> Gsl_matrix.matrix -> unit = "dediagonalize"
