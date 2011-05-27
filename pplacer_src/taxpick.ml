open Fam_batteries
open MapsSets

let underscoreize s =
  let s' = String.copy s in
  for i=0 to (String.length s')-1 do
    if s'.[i] = ' ' then s'.[i] <- '_'
  done;
  s'

let extract_tax_name decor =
  match List.filter (function | Decor.Taxinfo _ -> true | _ -> false) decor with
  | [Decor.Taxinfo (_,n)] -> n
  | _ -> assert(false)

let write_picks ~darr ~parr rp =
  let t = Refpkg.get_tax_ref_tree rp
  and name = Refpkg.get_name rp
  and model = Refpkg.get_model rp
  and mrcal = IntMap.keys (Refpkg.get_mrcam rp) in
  let ch = open_out (name^".picks") in
  let code = match Model.seq_type (Refpkg.get_model rp) with
  | Alignment.Nucleotide_seq -> Nuc_models.nuc_code
  | Alignment.Protein_seq -> Prot_models.prot_code
  in
  let get_symbol i =
    try code.(i) with | Invalid_argument _ -> assert(false)
  in
  let to_sym_str ind_arr =
    StringFuns.of_char_array (Array.map get_symbol ind_arr)
  in
  IntMap.iter
    (fun id (at_d, at_p) ->
      let uname =
        underscoreize (extract_tax_name (Gtree.get_bark t id)#get_decor)
      in
      Printf.fprintf ch ">%s\n%s\n" ("d_"^uname) (to_sym_str at_d);
      Printf.fprintf ch ">%s\n%s\n" ("p_"^uname) (to_sym_str at_p);)
    (Mutpick.pickpair_map Gsl_vector.max_index (-1) model t ~darr ~parr mrcal);
  close_out ch;
  ()
