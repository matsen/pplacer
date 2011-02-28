open Fam_batteries
open MapsSets

let underscoreize s =
  let s' = String.copy s in
  for i=0 to (String.length s')-1 do
    if s'.[i] = ' ' then s'.[i] <- '_'
  done;
  s'

let write_picks ~darr ~parr rp =
  let t = Refpkg.get_tax_gtree rp
  and name = Refpkg.get_name rp
  and model = Refpkg.get_model rp in
  Phyloxml.named_tree_to_file name t (name^".xml");
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
      match (Gtree.get_bark t id)#get_tax_nameo with
      | Some name ->
        let uname = underscoreize name in
        Printf.fprintf ch ">%s\n%s\n" ("d_"^uname) (to_sym_str at_d);
        Printf.fprintf ch ">%s\n%s\n" ("p_"^uname) (to_sym_str at_p);
      | None -> invalid_arg "unnamed MRCA!")
    (Mutpick.pickpair_map model t ~darr ~parr (Tax_gtree.mrca_list t));
  close_out ch;
  ()
