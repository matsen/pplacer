open Ppatteries

module BA = Bigarray
module BA1 = BA.Array1
module BA2 = BA.Array2
module BA3 = BA.Array3
module GA = BA.Genarray

let log_of_2 = log 2.

let safe_hashtbl_find h k =
  try Hashtbl.find h k with
  | Not_found -> failwith ("'"^k ^"' not found in the model!")

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

(* The sum of the unmasked entries of e. *)
let masked_total_twoexp e mask =
  assert(BA1.dim e = BA1.dim mask);
  let tot = ref 0. in
  for i=0 to BA1.dim e - 1 do
    if BA1.unsafe_get mask i <> 0 then
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

let seqtype_and_trans_statd_of_info
      model_name transitions empirical_freqs ref_align =
  if model_name = "GTR" then
    (Alignment.Nucleotide_seq,
     match transitions with
       | Some transitions ->
         (Nuc_models.b_of_trans_vector transitions,
          Alignment.emper_freq 4 Nuc_models.nuc_map ref_align)
       | None -> failwith "GTR specified but no substitution rates given.")
  else
    (Alignment.Protein_seq,
     let model_trans, model_statd =
       Prot_models.trans_and_statd_of_model_name model_name in
     (model_trans,
      if empirical_freqs then
        Alignment.emper_freq 20 Prot_models.prot_map ref_align
      else
        model_statd))

let gen_mmap_glv_arrays fd shared n_arrays n_glvs eba_dims aba_dims glv_cb =
  let dims = [|n_arrays; n_glvs|] in
  let aba = Array.append dims aba_dims
    |> GA.map_file fd BA.float64 BA.c_layout shared
  in
  let pos = GA.dims aba |> Array.enum |> Enum.fold ( * ) 8 |> Int64.of_int in
  let eba = Array.append dims eba_dims
    |> GA.map_file fd ~pos BA.int BA.c_layout shared
  in
  0 --^ n_arrays
  |> Enum.map (fun i ->
    0 --^ n_glvs
    |> Enum.map (fun j ->
      glv_cb (GA.slice_left eba [|i; j|]) (GA.slice_left aba [|i; j|]))
    |> Array.of_enum)
  |> Array.of_enum

let gen_size_of_glv_arrays eba_base aba_base n_arrays n_glvs =
  (* int and nativeint _should_ be the same size *)
  let int_bytes = Nativeint.size / 8 in
  let aba_elements = aba_base * n_arrays * n_glvs in
  let eba_elements = aba_elements * eba_base in
  (aba_elements * 8) + (eba_elements * int_bytes)
