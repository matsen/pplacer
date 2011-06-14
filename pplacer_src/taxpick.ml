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
  let uname id =
    underscoreize (extract_tax_name (Gtree.get_bark t id)#get_decor)
  in
  let distal_str_map =
    IntMap.map
      (fun (at_d, _) -> (to_sym_str at_d))
      (Mutpick.pickpair_map Gsl_vector.max_index (-1) model t ~darr ~parr mrcal)
  in
  let ch_picks = open_out (name^".picks") in
  let max_rat v = (Gsl_vector.max v) /. (Linear_utils.l1_norm v) in
  let ch_likes = open_out (name^".likes") in
  IntMap.iter
    (fun id (at_d, at_p) ->
      Printf.fprintf ch_likes ">%s\n" (uname id);
      Printf.fprintf ch_picks ">%s\n" (uname id);
      let distal_str = IntMap.find id distal_str_map in
      for i=0 to (Array.length at_d) - 1 do
        let d = at_d.(i) and p = at_p.(i) in
        if d > 0.8 then begin
          Printf.fprintf ch_likes "%d\t%g\t%g\n" i d p;
          output_char ch_picks distal_str.[i];
        end
        else output_char ch_picks '-';
      done;
      output_char ch_picks '\n';
      )
    (Mutpick.pickpair_map max_rat 0. model t ~darr ~parr mrcal);
  close_out ch_likes;
  close_out ch_picks;

  ()
