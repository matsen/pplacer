(* translating nucleotide sequences into likelihood vectors
 * *)

open Ppatteries

  (*
Adenine 	   A 	                A
Cytosine     	   C 	                C
Guanine            G 	                G
Thymine 	   T 	                T
Purine 	           G or A 	        R
Pyrimidine 	   T or C 	        Y
Amino 	           A or C               M
Keto 	           G or T 	        K
Strong (3H bonds)  G or C 	        S
Weak (2H bonds)    A or T 	        W
Not G 	           A or C or T 	        H
Not A 	           G or T or C 	        B
Not T 	           G or C or A 	        V
Not C 	           G or A or T 	        D
Any 	           G or C or T or A 	N
   *)

let nuc_map =
  CharMap.of_pairlist (
    List.map (fun (c, v) -> (c, Gsl_vector.of_array v)) (
(*            A   C   G   T  *)
      ['A', [|1.; 0.; 0.; 0.|];
       'C', [|0.; 1.; 0.; 0.|];
       'G', [|0.; 0.; 1.; 0.|];
       'T', [|0.; 0.; 0.; 1.|];
       'R', [|1.; 0.; 1.; 0.|];   (*  G or A 	         *)
       'Y', [|0.; 1.; 0.; 1.|];   (*  T or C 	         *)
       'M', [|1.; 1.; 0.; 0.|];   (*  A or C           *)
       'K', [|0.; 0.; 1.; 1.|];   (*  G or T 	         *)
       'S', [|0.; 1.; 1.; 0.|];   (*  G or C 	         *)
       'W', [|1.; 0.; 0.; 1.|];   (*  A or T 	         *)
       'H', [|1.; 1.; 0.; 1.|];   (*  A or C or T 	 *)
       'B', [|0.; 1.; 1.; 1.|];   (*  G or T or C 	 *)
       'V', [|1.; 1.; 1.; 0.|];   (*  G or C or A 	 *)
       'D', [|1.; 0.; 1.; 1.|];   (*  G or A or T 	 *)
       'N', [|1.; 1.; 1.; 1.|];   (*  G or C or T or A *)
       '-', [|1.; 1.; 1.; 1.|];
       '?', [|1.; 1.; 1.; 1.|];
       'X', [|1.; 1.; 1.; 1.|];]))

let nuc_code = [|'A';'C';'G';'T'|]

let lv_of_nuc nuc =
  try
    CharMap.find nuc nuc_map
  with
    | Not_found ->
        invalid_arg (Printf.sprintf "%c not a known nucleotide!" nuc)


(*
 * um, am i over-coding here?
 * coordinates of upper triangular matrix
 *
 # List.map transform 3 [0;1;2;3;4;5];;
- : (int * int) list = [(0, 1); (0, 2); (0, 3); (1, 2); (1, 3); (2, 3)]
 * *)
let transform dim x =
  let rec aux k i j =
    if j >= k then aux (k-1) (i+1) (j-k)
    else (i,i+j+1)
  in
  aux dim 0 x

let set_both m (i,j) x =
  m.(i).(j) <- x;
  m.(j).(i) <- x

(* make a symmetric matrix out of a vector which is assumed to be
 * ac ag at cg ct gt
 *)
let b_of_trans_vector v =
  assert(Array.length v = 6);
  let m = Array.make_matrix 4 4 0. in
  for i=0 to 5 do
    set_both m (transform 3 i) v.(i)
  done;
  Gsl_matrix.of_arrays m

