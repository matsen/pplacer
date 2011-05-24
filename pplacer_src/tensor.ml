module BA = Bigarray
module BA3 = BA.Array3

type tensor = (float, BA.float64_elt, BA.c_layout) BA.Array3.t

let dim1 = BA3.dim1
let dim2 = BA3.dim2
let dim3 = BA3.dim3
let dims x = (dim1 x, dim2 x, dim3 x)
let create a b c = BA3.create BA.float64 BA.c_layout a b c
let mimic a = create (BA3.dim1 a) (BA3.dim2 a) (BA3.dim3 a)
let copy a = let b = mimic a in BA3.blit a b; b
let set_all (* tensor value *) = BA3.fill

let ppr ff x =
  let n = dim1 x in
  Format.fprintf ff "@[{";
  for i=0 to n-1 do
    Linear_utils.ppr_gsl_matrix ff (BA3.slice_left_2 x i);
    if i < n-1 then Format.fprintf ff ";@ "
  done;
  Format.fprintf ff "}@]"

let fold_left f start x =
  let (d1, d2, d3) = dims x in
  let r = ref start in
  for i=0 to d1 - 1 do
    for j=0 to d2 - 1 do
      let v = BA3.slice_left_1 x i j in
      for k=0 to d3 - 1 do
        r := f !r v.{k}
      done
    done
  done;
  !r

(* Get the "worst" floating point classification in the tensor.
 * Note:
# max FP_normal FP_subnormal;;
- : fpclass = FP_subnormal
 * *)
let fp_classify x =
  fold_left (fun worst flt -> max worst (classify_float flt)) FP_normal x
