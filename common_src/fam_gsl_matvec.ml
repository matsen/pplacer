(* Copyright (C) 2009  Frederick A Matsen.
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * some nicer ways to make and use Gsl_vectors and Gsl_matrices.
 *
 * look at
 http://www.gnu.org/software/gsl/manual/html_node/GSL-BLAS-Interface.html
 and
 http://oandrieu.nerim.net/ocaml/gsl/doc/Gsl_blas.html
 *
 *)

let tolerance = 1e-13


(* making vectors and matrices *)

let vecMake n x = Gsl_vector.create ~init:x n
let matMake nrows ncols x = Gsl_matrix.create ~init:x nrows ncols

let vecOfArray a = 
  let n = Array.length a in
  let v = Gsl_vector.create n in
  for i=0 to n-1 do v.{i} <- a.(i) done;
  v

let vecOfList l = 
  vecOfArray (Array.of_list l)

let vecFold_left f start v = 
  let x = ref start
  and n = Gsl_vector.length v in
  for i=0 to n-1 do 
    x := f !x v.{i}
  done;
  !x

let vecInit n f = 
  let v = Gsl_vector.create n in
  for i=0 to n-1 do v.{i} <- f i done;
  v

let vecMimic v = Gsl_vector.create (Gsl_vector.length v)

let matInit nrows ncols f = 
  let m = Gsl_matrix.create nrows ncols in
  for r=0 to nrows-1 do
    for c=0 to ncols-1 do
      m.{r, c} <- f r c
    done;
  done;
  m

let allocMatTranspose m =
  let (rows, cols) = Gsl_matrix.dims m in
  matInit cols rows (fun i j -> m.{j,i})

let vecMap f v = 
  vecInit (Gsl_vector.length v)
             (fun i -> f v.{i})

let matMap f m = 
  let (rows, cols) = Gsl_matrix.dims m in
  matInit rows cols (fun i j -> f m.{i,j})

let vecMapFromArray f a = 
  let n = Array.length a in
  assert(n > 0);
  vecInit n ( fun i -> f a.(i) )

let matMapFromAAR f m = 
  (* assume rectangular *)
  let nrows = Array.length m in
  assert(nrows > 0);
  let ncols = Array.length m.(0) in
  assert(ncols > 0);
  matInit nrows ncols (
    fun i j -> f m.(i).(j)
  )

let diagOfArr a = 
  let n = Array.length a in
  let m = Gsl_matrix.create ~init:0. n n in
  for i=0 to n-1 do
    m.{i,i} <- a.(i)
  done;
  m

let diagOfVec v = 
  let n = Gsl_vector.length v in
  let m = Gsl_matrix.create ~init:0. n n in
  for i=0 to n-1 do
    m.{i,i} <- v.{i}
  done;
  m

(* information about vectors and matrices *)
let assertSymm m = 
  let n, cols = Gsl_matrix.dims m in
  assert(n = cols);
  for i=0 to n-1 do
    for j=i to n-1 do
      if (abs_float(m.{i,j} -. m.{j,i}) > tolerance) then
        failwith (
          Printf.sprintf 
            "matrix not symmetric: %f vs %f, difference of %g" 
            m.{i,j} m.{j,i} (m.{i,j} -. m.{j,i}))
    done
  done

let vecSatisfiesPredicate pred v = 
  vecFold_left 
    (fun soFar x -> soFar && pred x) 
    true 
    v

let vecNonneg v = 
  vecSatisfiesPredicate (fun x -> x >= 0.) v

let matVecMul dest a v = 
  Gsl_blas.gemv 
    Gsl_blas.NoTrans ~alpha:1. 
    ~a:a ~x:v ~beta:0. ~y:dest

let allocMatVecMul a v = 
  let (rows, midA) = Gsl_matrix.dims a 
  and midV = Gsl_vector.length v in
  assert(midA = midV);
  let w = Gsl_vector.create rows in
  matVecMul w a v;
  w

let matMatMul dest a b = 
  Gsl_blas.gemm 
    ~ta:Gsl_blas.NoTrans ~tb:Gsl_blas.NoTrans 
    ~alpha:1. ~a:a ~b:b ~beta:0. ~c:dest

let allocMatMatMul a b = 
  let (rows, midA) = Gsl_matrix.dims a
  and (midB, cols) = Gsl_matrix.dims b in
  assert(midA = midB);
  let m = Gsl_matrix.create rows cols in
  matMatMul m a b;
  m

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

(* eigen *)
let symmEigs m =
  assertSymm m;
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
