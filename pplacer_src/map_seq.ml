(* For calculating Maximum A Posteriori sequences for internal locations on the
 * tree. *)

open Fam_batteries
open MapsSets


type pos = Distal | Proximal

(* first we need to grab u1 and u2. These can be some snodes.(0) and snodes.(1)
 * (fail nicely if these are out of bounds, which means that the ref tree has <
 * 1 leaf. *)

let of_map u1 u2 model t ~darr ~parr m =
  let code = match Model.seq_type model with
  | Alignment.Nucleotide_seq -> Nuc_models.nuc_code
  | Alignment.Protein_seq -> Prot_models.prot_code
  in
  let get_symbol i =
    try code.(i) with | Invalid_argument _ -> assert(false)
  in
  let to_sym_str ind_arr =
    StringFuns.of_char_array (Array.map get_symbol ind_arr)
  in
  IntMap.mapi
    (fun id _ ->
      to_sym_str
        (Mutpick.get_summary
          Mutpick.Proximal
          Gsl_vector.max_index
          (-1) u1 u2 model t
          ~darr ~parr id))
    m

(* apply this one to the mrcam ... *)
