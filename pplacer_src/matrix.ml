open Ppatteries
include Bigarray.Array2
include Gsl.Matrix

let create a b = Bigarray.Array2.create Bigarray.float64 Bigarray.c_layout a b
let mimic a = create (dim1 a) (dim2 a)

(* Get the "worst" floating point classification in the tensor.
 * Note:
# max FP_normal FP_subnormal;;
- : fpclass = FP_subnormal
 * *)
let fp_classify x =
  enum x
  |> Enum.fold (fun worst flt -> max worst (classify_float flt)) FP_normal

let ppr ff m =
  let last = dim1 m - 1 in
  Format.fprintf ff "@[{";
  for i=0 to last do
    Linear_utils.ppr_gsl_vector ff (row m i);
    if i < last then Format.fprintf ff ";@ "
  done;
  Format.fprintf ff "}@]"

let rect_transpose m =
  let m' = create (dim2 m) (dim1 m) in
  modifyij
    (fun i j _ -> get m j i)
    m';
  m'

