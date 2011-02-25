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


(* these are slow; just for checking the c versions. *)

let ocaml_gemmish dst a b =
  let (max_i,max_k) = Gsl_matrix.dims a
  and (max_k',max_j) = Gsl_matrix.dims b
  in
  assert(max_k = max_k');
  assert((max_i,max_j) = Gsl_matrix.dims dst);
  for i=0 to max_i-1 do
    for j=0 to max_j-1 do
      dst.{i,j} <- 0.;
      for k=0 to max_k-1 do
        dst.{i,j} <- dst.{i,j} +. a.{i,k} *. b.{k,j}
      done
    done
  done

let ocaml_dediagonalize dst u lambda uit =
  let n = Gsl_vector.length lambda in
  for i=0 to n-1 do
    for j=0 to n-1 do
      dst.{i,j} <- 0.;
      for k=0 to n-1 do
        dst.{i,j} <- dst.{i,j} +. lambda.{k} *. u.{i,k} *. uit.{j,k}
      done
    done
  done
