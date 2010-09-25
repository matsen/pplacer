(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Fam_batteries
open MapsSets

let underscoreize s = 
  let s' = String.copy s in
  for i=0 to (String.length s')-1 do
    if s'.[i] = ' ' then s'.[i] <- '_'
  done;
  s'

let write_picks ~darr ~parr rp = 
  let t = Tax_gtree.of_refpkg rp 
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
