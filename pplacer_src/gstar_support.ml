open Ppatteries

module BA = Bigarray
module BA1 = BA.Array1
module BA2 = BA.Array2
module BA3 = BA.Array3

let log_of_2 = log 2.

(* integer big arrays *)
let iba1_create = BA1.create BA.int BA.c_layout
let iba1_mimic a = iba1_create (BA1.dim a)
let iba1_copy a = let b = iba1_mimic a in BA1.blit a b; b
let iba1_to_array a =
  let arr = Array.make (BA1.dim a) 0 in
  for i=0 to (BA1.dim a)-1 do arr.(i) <- a.{i} done;
  arr
let iba1_ppr ff a = Ppr.ppr_int_array ff (iba1_to_array a)
let iba1_pairwise_sum dest x y =
  let n = BA1.dim x in
  assert(n = BA1.dim y && n = BA1.dim dest);
  for i=0 to n-1 do
    BA1.unsafe_set
      dest i ((BA1.unsafe_get x i) + (BA1.unsafe_get y i))
  done

(* gets the base two exponent *)
let get_twoexp x = snd (frexp x)

(* makes a float given a base two exponent. we use 0.5 because:
   # frexp (ldexp 1. 3);;
   - : float * int = (0.5, 4)
   so that's how ocaml interprets 2^i anyway.
*)
let of_twoexp i = ldexp 0.5 (i+1)

(* total all of the stored exponents. we use a float to avoid overflow. *)
let total_twoexp e =
  let last = BA1.dim e - 1
  and tot = ref 0. in
  for i=0 to last do
    tot := !tot +. float_of_int (BA1.unsafe_get e i)
  done;
  !tot

(* total all of the stored exponents in a specified range. *)
let bounded_total_twoexp e start last =
  let tot = ref 0. in
  for i=start to last do
    tot := !tot +. float_of_int (BA1.unsafe_get e i)
  done;
  !tot

(* multiply by a tensor *)
let tensor_mul tensor ~dst ~src =
  let last = Tensor.dim1 src - 1 in
  (* iter over rates *)
  for i=0 to last do
    let src_mat = BA3.slice_left_2 src i
    and evo_mat = BA3.slice_left_2 tensor i
    and dst_mat = BA3.slice_left_2 dst i
    in
    Linear.gemmish dst_mat evo_mat src_mat
  done
