(* just the calls for linear_c.
*)

module BA = Bigarray
module BA1 = BA.Array1
module BA2 = BA.Array2

type uint16_vector = (int, BA.int16_unsigned_elt, BA.c_layout) BA1.t

(* *** Matrices, used for transformation. *** *)

(* dst = a * b *)
external gemmish : Gsl_matrix.matrix -> Gsl_matrix.matrix -> Gsl_matrix.matrix -> unit = "gemmish_c"

(* dst u lambda uit
 * where uit is u inverse transpose
 * dst_ij = sum_k (lambda_k *. u_ik *. uit_jk)
 * *)
external dediagonalize : Gsl_matrix.matrix -> Gsl_matrix.matrix -> Gsl_vector.vector -> Gsl_matrix.matrix -> unit = "dediagonalize"


(* *** Matrices, that are used for gcat. *** *)

(* print *)
external mat_print : Gsl_matrix.matrix -> unit = "mat_print_c"

(* statd x y z util *)
external mat_log_like3 : Gsl_vector.vector -> Gsl_matrix.matrix -> Gsl_matrix.matrix -> Gsl_matrix.matrix -> float = "mat_log_like3_c"

(* dst x y *)
external mat_pairwise_prod : Gsl_matrix.matrix -> Gsl_matrix.matrix -> Gsl_matrix.matrix -> unit = "mat_pairwise_prod_c"

(* statd dst a b *)
external mat_statd_pairwise_prod : Gsl_vector.vector -> Gsl_matrix.matrix -> Gsl_matrix.matrix -> Gsl_matrix.matrix -> unit = "mat_statd_pairwise_prod_c"

(* x y mask
 * Take the across-sites sum of the logarithm of the per-site dot products of x
 * and y restricted to the sites that have a nonzero value in the mask.
 * *)
external mat_masked_logdot : Gsl_matrix.matrix -> Gsl_matrix.matrix -> uint16_vector -> float = "mat_masked_logdot_c"

(* x y first last
 * take the logarithm of the dot product of x and y restricted to the interval
 * [start, last]. start and last are 0-indexed, of course.
 * *)
external mat_bounded_logdot : Gsl_matrix.matrix -> Gsl_matrix.matrix -> int -> int -> float = "mat_bounded_logdot_c"


(* *** Tensors, that are used for gmix. *** *)

(* print *)
external ten_print : Tensor.tensor -> unit = "ten_print_c"

(* statd x y z util *)
external ten_log_like3 : Gsl_vector.vector -> Tensor.tensor -> Tensor.tensor -> Tensor.tensor -> Gsl_vector.vector -> float = "ten_log_like3_c"

(* dst x y *)
external ten_pairwise_prod : Tensor.tensor -> Tensor.tensor -> Tensor.tensor -> unit = "ten_pairwise_prod_c"

(* statd dst a b *)
external ten_statd_pairwise_prod : Gsl_vector.vector -> Tensor.tensor -> Tensor.tensor -> Tensor.tensor -> unit = "ten_statd_pairwise_prod_c"

(* x y mask util
 * Take the logarithm of the dot product of x and y restricted to the sites
 * that have a nonzero value in the mask.
 * *)
external ten_masked_logdot : Tensor.tensor -> Tensor.tensor -> uint16_vector -> Gsl_vector.vector -> float = "mat_masked_logdot_c"

(* x y first last util
 * take the logarithm of the dot product of x and y restricted to the interval
 * [start, last]. start and last are 0-indexed, of course.
 * *)
external ten_bounded_logdot : Tensor.tensor -> Tensor.tensor -> int -> int -> Gsl_vector.vector -> float = "ten_bounded_logdot_c"

(* vec_pairwise_prod dst x y *)
external vec_pairwise_prod : Gsl_vector.vector -> Gsl_vector.vector -> Gsl_vector.vector -> unit = "vec_pairwise_prod_c"

(* int_vec_tot x
 * The total of an integer vector. *)
external int_vec_tot : uint16_vector -> int = "int_vec_tot_c"

(* int_vec_pairwise_prod dst x y *)
external int_vec_pairwise_prod : uint16_vector -> uint16_vector -> uint16_vector  -> unit = "int_vec_pairwise_prod_c"

(* float_mat_int_vec_mul dest mat vec
 * Left multiply the integer vector by the float matrix. This routine
 * specializes in being fast when the integer is sparse.
 * *)
external float_mat_int_vec_mul : Gsl_vector.vector -> Gsl_matrix.matrix -> uint16_vector -> unit = "float_mat_int_vec_mul_c"
