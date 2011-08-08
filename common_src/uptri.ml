open Ppatteries
(* a module for STRICTLY uppper tri matrices *)

(* some utils *)
let n_entries_of_dim dim = dim * (dim-1) / 2

(*
  dim_of_nentries :
  * convert the number of entries into the number of dimensions. note that zero
  * entries is a 1x1, one is a 2x2, etc.
*)
let dim_of_n_entries n_entries =
  let rec aux dim_sofar left =
    if left = 0 then
      dim_sofar
    else if left < 0 then
      invalid_arg "dim_of_nentries : not a dimension"
    else
      aux (dim_sofar+1) (left-dim_sofar-1)
  in
  (* one plus to add in the diagonal *)
  1+(aux 0 n_entries)

(* pair_to_int :
  * convert a pair representing the 0-indexed coordinates of a matrix entry to
  * the zero-indexed number in their sequence, i.e.
  * 0 1 2 3
  * * 4 5 6
  * * * 7 8
  * * * * 9
  * * * * *
  *)
let pair_to_int dim i j = j-1 + i*(2*dim-i-3)/2

let rec int_to_pair dim k =
  let rec aux curr_i start_j curr_k =
    assert(start_j < dim);
    assert(0 <= curr_i && curr_i < dim);
    assert(curr_k >= 0);
    if start_j+curr_k < dim then (curr_i, start_j+curr_k)
    else aux (curr_i+1) (start_j+1) (curr_k-(dim-start_j))
  in
  (* below: 0 1 so we start at (0,1) *)
  aux 0 1 k

(* interface *)

type 'a uptri = {dim: int; data: 'a array}

let get_dim u = u.dim
let get_data u = u.data

let dims_ok u i j =
  0 <= i && i < u.dim && i < j && j < u.dim
let assert_dims_ok u i j =
  if not (dims_ok u i j) then
    invalid_arg (Printf.sprintf "uptri : dims %d %d in %d not OK" i j u.dim)

let create d x = {dim = d; data = Array.create (n_entries_of_dim d) x}
let size u = u.dim
let get u i j = assert_dims_ok u i j; u.data.(pair_to_int u.dim i j)
let set u i j x = assert_dims_ok u i j; u.data.(pair_to_int u.dim i j) <- x
let unsafe_get u i j = Array.unsafe_get u.data (pair_to_int u.dim i j)
let unsafe_set u i j x = Array.unsafe_set u.data (pair_to_int u.dim i j) x
let to_array u = Array.copy u.data
let of_array a = {dim = dim_of_n_entries (Array.length a); data = a}
let row u i = Array.init (u.dim-i-1) (fun j -> get u i (j+1+i))
let fold_left f s u = Array.fold_left f s u.data
let map f u = {u with data = Array.map f u.data}
let iter f u = Array.iter f u.data

let iterij f u =
  Array.iteri
    (fun k x -> let (i,j) = int_to_pair u.dim k in f i j x)
    u.data

let compare u1 u2 =
  if u1.dim <> u2.dim then invalid_arg "Uptri.compare : differing dimensions";
  compare u1.data u2.data

let init d f =
  {dim = d; data =
    Array.init (n_entries_of_dim d) (
      fun k -> let (i,j) = int_to_pair d k in f i j)}

let apply_pairwise f u1 u2 =
  if u1.dim <> u2.dim then invalid_arg "Uptri.apply_pairwise : differing dimensions";
  {u1 with data = ArrayFuns.map2 f u1.data u2.data}
let number dim = init dim (fun i j -> j+dim*i)

(* get as if it was an everything-but-diagonal matrix *)
let get_loose u i j =
  if i=j then invalid_arg "get_loose : on diagonal"
  else if i < j then get u i j
  else get u j i

(* set as if it was an everything-but-diagonal matrix *)
let set_loose u i j x =
  if i=j then invalid_arg "set_loose : on diagonal"
  else if i < j then set u i j x
  else set u j i x

let pair_to_int_loose dim i j =
  if i=j then invalid_arg "pair_to_int_loose : on diagonal"
  else if i < j then pair_to_int dim i j
  else pair_to_int dim j i

let to_matrix diag_f u =
  MatrixFuns.init u.dim u.dim
    (fun i j -> if i = j then diag_f i else get_loose u i j)

(* pprs *)
let ppr_uptri print_val ff u =
  Ppr.ppr_array (Ppr.ppr_array print_val) ff (
      Array.init (u.dim-1) (fun i -> row u i))
(* let ppr_uptri print_val ff u = Ppr.ppr_array print_val ff u.data  *)
let ppr_int_uptri ff u = ppr_uptri Format.pp_print_int ff u
let print_int_uptri u = Ppr.print_of_ppr ppr_int_uptri u
let ppr_float_uptri ff u = ppr_uptri Ppr.ppr_gfloat ff u
let print_float_uptri u = Ppr.print_of_ppr ppr_float_uptri u

(* print as a lower triangular matrix to a channel (nicer to look at) *)
let ppr_lowtri ff ppr u =
  let d = get_dim u in
  for i=1 to d-1 do
    for j=0 to i-1 do
      Format.fprintf ff "%a\t" ppr (get_loose u i j);
    done;
    Format.fprintf ff "@\n";
  done


