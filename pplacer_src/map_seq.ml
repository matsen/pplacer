(* For calculating Maximum A Posteriori sequences for internal locations on the
 * tree. *)

open Fam_batteries
open MapsSets
open Tax_id

let flip f x y = f y x

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

let mrca_seq_map locmap mrcam tree =
  let rec aux accum = function
    | [] -> accum
    | (top_mrca, tree) :: rest ->
      let i = Stree.top_id tree in
      let accum' = match top_mrca with
        | Some top_mrca ->
          List.fold_left
            (flip (IntMap.add_listly top_mrca))
            accum
            (IntMap.get i [] locmap)
        | None -> accum
      and top_mrca' = if IntMap.mem i mrcam then Some i else top_mrca in
      let rest' = match tree with
        | Stree.Node (_, subtrees) ->
          List.fold_left
            (fun accum subtree -> (top_mrca', subtree) :: accum)
            rest
            subtrees
        | Stree.Leaf _ -> rest
      in
      aux accum' rest'
  in
  aux IntMap.empty [None, tree]

