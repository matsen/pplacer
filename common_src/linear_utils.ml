(* Some nicer ways to make and use Gsl_vectors and Gsl_matrices.
 *
 * look at
 http://www.gnu.org/software/gsl/manual/html_node/GSL-BLAS-Interface.html
 and
 http://oandrieu.nerim.net/ocaml/gsl/doc/Gsl_blas.html
 *
 * Reminder: the Gsl_matrix is made with c_layout.
 *)

let tolerance = 1e-15

open Bigarray


(* *** Vector operations *** *)

let vec_init n f =
  let v = Gsl_vector.create n in
  for i=0 to n-1 do Array1.unsafe_set v i (f i) done;
  v

let vec_fold_left f start v =
  let x = ref start
  and n = Gsl_vector.length v in
  for i=0 to n-1 do
    x := f !x (Array1.unsafe_get v i)
  done;
  !x

let vec_map f v =
  vec_init (Gsl_vector.length v) (fun i -> f (Array1.unsafe_get v i))

let vec_iter f a =
  let n = Gsl_vector.length a in
  for i=0 to n-1 do
    f (Array1.unsafe_get a i)
  done

let vec_iteri f a =
  let n = Gsl_vector.length a in
  for i=0 to n-1 do
    f i (Array1.unsafe_get a i)
  done

let vec_iter2 f a b =
  let n = Gsl_vector.length a in
  assert(n = Gsl_vector.length b);
  for i=0 to n-1 do
    f (Array1.unsafe_get a i) (Array1.unsafe_get b i)
  done

let vec_map2_into f ~dst a b =
  let n = Gsl_vector.length dst in
  assert(n = Gsl_vector.length a);
  assert(n = Gsl_vector.length b);
  for i=0 to n-1 do
    Array1.unsafe_set dst i
      (f (Array1.unsafe_get a i) (Array1.unsafe_get b i))
  done

(* If all of the entries of v satisfy pred. *)
let vec_predicate pred v =
  vec_fold_left (fun so_far x -> so_far && pred x) true v

(* Maximum element after applying f. *)
let vec_fmax_index f v =
  assert(0 <> Gsl_vector.length v);
  let max_val = ref (f v.{0})
  and max_ind = ref 0
  in
  vec_iteri
    (fun i x ->
      let fx = f x in
      if fx > !max_val then begin
        max_val := fx;
        max_ind := i;
      end)
    v;
  !max_ind

(* norms and normalizing *)
let lp_norm p v =
  assert(p > 0.);
  let x = ref 0. in
  for i=0 to (Gsl_vector.length v)-1 do
    x := !x +. (v.{i} ** p)
  done;
  !x ** (1. /. p)

let l1_norm v = lp_norm 1. v
let l2_norm v = lp_norm 2. v

(* normalize in place *)
let gen_normalize norm_fun v =
  Gsl_vector.scale v (1. /. (norm_fun v))
let l1_normalize v = gen_normalize l1_norm v
let l2_normalize v = gen_normalize l2_norm v

let alloc_gen_normalize norm_fun v =
  let normed = Gsl_vector.copy v in
  Gsl_vector.scale normed (1. /. (norm_fun normed));
  normed
let alloc_l1_normalize v = alloc_gen_normalize l1_norm v
let alloc_l2_normalize v = alloc_gen_normalize l2_norm v


(* *** Matrix operations *** *)

let mat_init n_rows n_cols f =
  let m = Gsl_matrix.create n_rows n_cols in
  for i=0 to n_rows-1 do
    let row = Array2.slice_left m i in
    for j=0 to n_cols-1 do
      Array1.unsafe_set row j (f i j)
    done;
  done;
  m

let mat_map f m =
  let (rows, cols) = Gsl_matrix.dims m in
  mat_init rows cols (fun i j -> f m.{i,j})

let diag v =
  let n = Gsl_vector.length v in
  let m = Gsl_matrix.create ~init:0. n n in
  for i=0 to n-1 do
    m.{i,i} <- v.{i}
  done;
  m

(* information about vectors and matrices *)
let assert_symmetric m =
  let n, cols = Gsl_matrix.dims m in
  assert(n = cols);
  for i=0 to n-1 do
    for j=i to n-1 do
      if (abs_float(m.{i,j} -. m.{j,i}) > tolerance) then
        failwith (
          Printf.sprintf
            "matrix not symmetric: %f vs %f" m.{i,j} m.{j,i})
    done
  done

let alloc_transpose m =
  let mt = Gsl_matrix.copy m in
  Gsl_matrix.transpose_in_place mt;
  mt

let mat_dim_asserting_square m =
  let n = Array2.dim1 m in
  assert(n = Array2.dim2 m);
  n

let qform m v =
  let n = Gsl_vector.length v in
  assert(n = Array2.dim1 m && n = Array2.dim2 m);
  let x = ref 0. in
  for i=0 to n-1 do
    let vi = Array1.unsafe_get v i
    and mi = Array2.slice_left m i
    in
    for j=0 to n-1 do
      x := (!x) +. vi *. (Array1.unsafe_get v j) *. (Array1.unsafe_get mi j)
    done;
  done;
  !x

let trace m =
  let n = Array2.dim1 m in
  assert(n = (Array2.dim2 m));
  let x = ref 0. in
  for i=0 to n-1 do
      x := (!x) +. (Array2.unsafe_get m i i)
  done;
  !x

let mat_vec_mul dest a v =
  Gsl_blas.gemv
    Gsl_blas.NoTrans ~alpha:1.
    ~a:a ~x:v ~beta:0. ~y:dest

let alloc_mat_vec_mul a v =
  let (rows, midA) = Gsl_matrix.dims a
  and midV = Gsl_vector.length v in
  assert(midA = midV);
  let w = Gsl_vector.create rows in
  mat_vec_mul w a v;
  w

let mat_mat_mul dest a b =
  Gsl_blas.gemm
    ~ta:Gsl_blas.NoTrans ~tb:Gsl_blas.NoTrans
    ~alpha:1. ~a:a ~b:b ~beta:0. ~c:dest

let alloc_mat_mat_mul a b =
  let (rows, midA) = Gsl_matrix.dims a
  and (midB, cols) = Gsl_matrix.dims b in
  assert(midA = midB);
  let m = Gsl_matrix.create rows cols in
  mat_mat_mul m a b;
  m

(* gives a matrix such that the columns are the eigenvectors. *)
let symm_eigs m =
  assert_symmetric m;
  Gsl_eigen.symmv (`M(m))

(* pretty printers *)
let ppr_gsl_vector ff y =
  Format.fprintf ff "@[{";
  Ppr.ppr_list_inners Format.pp_print_float ff (
    Array.to_list (Gsl_vector.to_array y));
  Format.fprintf ff "}@]"

let ppr_gsl_matrix ff m =
  let nrows, _ = Gsl_matrix.dims m in
  Format.fprintf ff "@[{";
  for i=0 to nrows-1 do
    ppr_gsl_vector ff (Gsl_matrix.row m i);
    if i < nrows-1 then Format.fprintf ff ";@ "
  done;
  Format.fprintf ff "}@]"
